##### Load packages and functions #####
#install.packages("pacman")
library(pacman)
p_load(haven, tidyr, dplyr, ggplot2, psych, questionr, scales, gridExtra, data.table, Weighted.Desc.Stat, car, xlsx)

source("freq_q.R")
source("MVA-functions.R")


##### Load data #####
# Load z read_sav (potrebujemo haven 1.1.0. (1.1.1. dela težave))
WV6 <- as.data.table(read_sav("data/WV6_Data_spss_v_2016_01_01.sav"))

# izberemo samo mehiko in švedsko
mex_swe <- filter(WV6, V2 == 484 | V2 == 752)

# rekodiranje družbenega razreda 
mex_swe$V238_R <- car::Recode(mex_swe$V238, "1:2 = 1; 3 = 2; 4 = 3; 5 = 4; else = NA", as.factor = TRUE)

# 3206 vseh enot
schwartz <- select(mex_swe, V70:V79, -V74B)
nrow(schwartz)

# logični vektor izbite enot - potrebujemo za izbiro podatkov za ANOVO
selected_cases <- complete.cases(schwartz)

# 3143 enot, ki imajo vse vrednosti (brez NA)
schwartz <- schwartz[complete.cases(schwartz), ]
nrow(schwartz)

# standardiziramo spremenljivke
sch_scale <- scale(schwartz)

# izračunamo evklidsko razdaljo med enotami
sch_dist <- dist(sch_scale, method = "euclidian")

# Dendrogram po wardowi metodi
sch_hier_ward <- hclust(sch_dist, method = "ward.D2")
plot(sch_hier_ward, hang = -1)
rect.hclust(sch_hier_ward, k = 2, border = "red")


## KAKO IZRAČUNATI SUM-OF-SQUARES ??


# Dendrogram po minimalni metodi
sch_hier_single <- hclust(sch_dist, method = "single")
plot(sch_hier_single, hang = -1)

# Dendrogram po maksimalni metodi
sch_hier_compl <- hclust(sch_dist, method = "complete")
plot(sch_hier_compl, hang = -1)


# Povprečja schwartzovih spremenljivk po skupinah wardove metode
sch_scale <- as.data.table(sch_scale)
sch_scale$ward_skup <- cutree(sch_hier_ward, k = 2)
means <- sch_scale[, lapply(.SD, mean), by = ward_skup]
sd <- sch_scale[, lapply(.SD, sd), by = ward_skup]
N <- sch_scale[, lapply(.SD, length), by = ward_skup]

# df ... podatki
# key ... ime nove variable v kateri imena stolpcev
# value ... ime nove variable v kateri so shranjeni podatki (npr. povprečja spremenljivk)
means <- means %>% gather(variabla, mean, V70:V79, factor_key=TRUE) %>% select(2,1,3)
sd <- sd %>% gather(variabla, sd, V70:V79, factor_key=TRUE) %>% select(2,1,3)
N <- N %>% gather(variabla, N, V70:V79, factor_key=TRUE) %>% select(2,1,3)

# združimo podatke povprečjih, standardnih odlonih in numerusu
stats <- as.data.table(cbind(means, sd[3], N[3]))

# dodamo podatek o standardni napaki: SE
stats[, SE := (qt(0.975, df = N-1)*sd)/sqrt(N)]

# graf povprečij standardiziranih schwartzovih spremenljivk
slika3.1.4 <- ggplot(stats, aes(x=variabla, y=mean, group=ward_skup, color=ward_skup)) +
                      geom_point(show.legend = FALSE) +
                      geom_line(show.legend = FALSE) +
                      scale_y_continuous(limits = c(-0.75,0.75)) +
                      geom_errorbar(ymin=stats$mean-stats$SE, ymax=stats$mean+stats$SE, show.legend = FALSE)

# v df zapišemo za vsako enoto, kateri skupini smo jo dodelili
schwartz$ward_skup <- cutree(sch_hier_ward, k = 2)

# izračunamo povprečja po skupinah za spremenljivke
schwartz <- as.data.table(schwartz)
means_nonScale <- schwartz[, lapply(.SD, mean), by = ward_skup]

# izračunamo Numeruse po skupinah za spremenljivke
N_nonScale <- schwartz[, lapply(.SD, length), by = ward_skup]

# transformiramo df v format, kot je v Wordu
means_nonScale <- means_nonScale %>% gather(variabla, mean, V70:V79, factor_key=TRUE) %>% select(2,1,3)
N_nonScale <- N_nonScale %>% gather(variabla, N, V70:V79, factor_key=TRUE) %>% select(2,1,3)

means_nonScale <- means_nonScale %>% spread(ward_skup, mean)
N_nonScale <- N_nonScale %>% spread(ward_skup, N)

# numerusi in povprečja NESTANDARDIZIRANIH schwartzovih spremenljivk po skupinah
tabela3.1.4 <- cbind(N_nonScale, means_nonScale[c(2:3)])
names(tabela3.1.4) <- c("variabla", "N_Visoko", "N_Nizko", "mean_Visoko", "mean_Nizko")


# transformiramo df v format, kot je v Wordu
means <- means %>% spread(ward_skup, mean)
N <- N %>% spread(ward_skup, N)

# numerusi in povprečja STANDARDIZIRANIH schwartzovih spremenljivk po skupinah
tabela3.1.5 <- cbind(N, means[c(2:3)])
names(tabela3.1.5) <- c("variabla", "N_Visoko", "N_Nizko", "mean_Visoko", "mean_Nizko")


##### K-MEANS #####
schwartz <- schwartz[, -11]
kmeans2 <- kmeans(schwartz, centers=2, nstart=1000)

# povprečja schwartzovih spremenljivk po skupinah
tabela3.2.1 <- t(kmeans2$centers)
tabela3.2.1 <- as.data.table(tabela3.2.1)
tabela3.2.1[, "variabla"] <- paste0("V", 70:79) 
names(tabela3.2.1) <- c("skupina_1", "skupina_2", "variabla")

# končna tabela tabela3.2.1
tabela3.2.1 <- tabela3.2.1 %>% select(3,1,2)

# odmiki od celotnega povprečja schwartzovih spremenljivk po skupinah
tabela3.2.2 <- cbind(tabela3.2.1, mean=sapply(schwartz, mean))
tabela3.2.2[, "odmik_1"] <- tabela3.2.2[,2] - tabela3.2.2[,4]
tabela3.2.2[, "odmik_2"] <- tabela3.2.2[,3] - tabela3.2.2[,4]

# končna tabela tabela3.2.2
tabela3.2.2 <- tabela3.2.2[,c(1,5,6)]

# velikosti skupin 
tabela3.2.3 <- c(skupina_1=kmeans2$size[1], 
                 skupina_2=kmeans2$size[2], 
                 valid=sum(kmeans2$size), 
                 missing = nrow(mex_swe)-sum(kmeans2$size))

# vrednot kriterijske funkcije na 1000 ponovitvah
tabela3.2.4 <- kmeans2$tot.withinss

# vrednost kriterijske funkcije na 100 iteracijah 1 ponovitve
tabela3.2.5 <- list()
kmeans_iter <- list()
i <- 1
while (i < 101) {
  tabela3.2.5[[i]] <- kmeans(schwartz, centers=2, nstart=1)$tot.withinss
  i <- i+1
  
  kmeans_iter[[i]] <- kmeans(schwartz, centers=2, nstart=1)$cluster
}

tabela3.2.5 <- data.table(iteracija = seq(1:100), 
                          SumOfSquares = unlist(tabela3.2.5))

# kriterijska funckcija ima pri vseh 100 ponovitvah enako vrednost
tabela3.2.5 <- tabela3.2.5[order(tabela3.2.5$SumOfSquares)]

# glede na to, da je vrednost kriterijske funkcije pri obeh različicah enaka, bom upošteval razdelitev, ki smo jo dobili po 1000 ponovitvah in je shranjena v variabli kmeans2

# izračunamo povprečja po skupinah za spremenljivke
schwartz <- as.data.table(schwartz)

# v df zapišemo podatek kateri skupini pripada enota s kmeans 
schwartz$kmeans <- kmeans2$cluster

means_N <- function(df, skupina) {
  # df = tabela v obliki data.table
  # skupina = ime variable s podatki o pripadnosti skupini v character obliki (npr. "ime_variable")
  
  if (class(df)[1] != "data.table") {
    df <- as.data.table(df)
  } 
  
  if (class(skupina) != "character") {
    skupina <- as.character(skupina)
  }
  
  # izračuna skupino
  means <- df[, lapply(.SD, mean), by = skupina]
  if (length(means) == 0) {
    warning("check means!")
  }
  
  N <- df[, lapply(.SD, length), by = skupina]
  if (length(N) == 0) {
    warning("check N!")
  }
  
  means <- means %>% gather(variabla, mean, V70:V79, factor_key=TRUE) %>% select(2,1,3) %>% spread(skupina, mean)
  N <- N %>% gather(variabla, N, V70:V79, factor_key=TRUE) %>% select(2,1,3) %>% spread(skupina, N)
  
  if (nrow(means) == 0 | nrow(N) == 0) {
    warning("prazna tabela means ali nrow!")
  } else if (nrow(means) != nrow(N)) {
    warning("tabela povprečij in tabela numerusov nista enako dolgi!")
  }
  
  output <- cbind(N, means[c(2:3)])
  
  return(output)
}

tabela3.2.6 <- means_N(schwartz, "kmeans")
names(tabela3.2.6) <- c("variabla", "N_Visoko", "N_Nizko", "mean_Visoko", "mean_Nizko")

# odmiki od celotnega povprečja schwartzovih spremenljivk po skupinah
tabela3.2.7 <- cbind(tabela3.2.6, mean=sapply(schwartz, mean)[-11])
tabela3.2.7[, "odmik_1"] <- tabela3.2.7[,4] - tabela3.2.7[,6]
tabela3.2.7[, "odmik_2"] <- tabela3.2.7[,5] - tabela3.2.7[,6]

# končna tabela tabela3.2.7
tabela3.2.7 <- tabela3.2.7[,c(1:3,7,8)]

# primerjava razbitij (1x1000 ponovitev, 100x1 ponovitev)
# glede na to, da je vrednost krit. funk. pri 1 ponovitvi vsepovsod enaka izberemo kar npr. 100 razbitje
kmeans_clustering <- cbind(kmeans_1000=schwartz$kmeans,
                           kmeans_1=kmeans_iter[[100]]) 

# primerjava razbitij med seboj
xtabs(~ kmeans_1000+kmeans_1, data = kmeans_clustering)

# koeficient negotovosti
p_load(ryouready)
nom.uncertainty(kmeans_clustering)

# likertovi lestvici: schwartz
likert <- schwartz[,-11]

likert <- likert[, .(nase=(V70+V71+V73+V75+V76)/5,
                       druge=(V72+V74+V77+V78+V79)/5)]

likert <- cbind(likert, schwartz[,11])

slika3.2.1 <- ggplot(likert, aes(druge, nase, color = as.character(kmeans))) +
                geom_jitter(show.legend = TRUE) +
                scale_x_continuous(limits = c(0,7)) +
                scale_y_continuous(limits = c(0,7)) +
                scale_color_discrete(name="Kmeans\nskupine",
                                     labels=c("Nizko", "Visoko"))







##### Povezanost usmerjenosti s starostjo #####
# df spremenimo v dt za lažji subset
mex_swe_dt <- as.data.table(mex_swe)

# priprava podatkov: fetching starost in podatke o pripadnosti skupini
skupine <- schwartz$kmeans
starost <- mex_swe[selected_cases, "V242"]

# data.table s podatki o starosti in skupinah
star_skup <- as.data.table(cbind(starost, skupine))

# deskriptivna statistika o starosti skupine kmeans
# rbind statistike po skupinah in total statistik
tabela3.3.1 <- rbind(describeBy(star_skup$starost, star_skup$skupine, mat = TRUE)[,-1], 
                     cbind(group1 = "total", psych::describe(star_skup$starost)))
setDT(tabela3.3.1)

# ANOVA - odvisna je starost, neodvisna so skupine
tabela3.3.2 <- as.data.frame(summary(aov(starost ~ skupine, data = star_skup))[[1]])









##### Povezanost usmerjenosti z državo #####
drzava <- mex_swe[selected_cases, "V2"]
drzava_skup <- as.data.table(cbind(drzava, skupine))

# kontingenčna tabela spremenljivk država in skupine
x <- addmargins(xtabs(~drzava+skupine, data = drzava_skup))

# tabela z deleži zgornje tabele
xx <- cbind(round(prop.table(x[,-3], 1)*100, 1),
            Sum = rep(100, nrow(x)))

# oblikovana kontingenčna tabela 
tabela3.3.3 <- as.data.table(rbind(x[1,], xx[1,], x[2,], xx[2,], x[3,], xx[3,]))
tabela3.3.3 <- cbind(drzava = c("Mexico", "Mexico", "Sweden", "Sweden", "Total", "Total"), tabela3.3.3)
colnames(tabela3.3.3)[c(2:4)] <- c("Nizko", "Visoko", "Total")

# povezanost države in skupin
tabela3.3.4 <- rbind(hi_hvadrat = c(chisq.test(x[-3,-3])$statistic, chisq.test(x[-3,-3])$p.value),
                     continuity_corr = c(prop.test(x[-3,-3], correct=TRUE)$statistic, prop.test(x[-3,-3], correct=TRUE)$p.value),
                     fisher = c(NA, fisher.test(x, simulate.p.value=TRUE)$p.value))

colnames(tabela3.3.4) <- c("vrednost", "p_vrednost")

# koeficienta Cramer V in kontingenčni koef. sta sumljivo nizka
p_load(vcd)
tabela3.3.5 <- assocstats(x[-3,-3])










##### Povezanost usmerjenosti s spolom #####
spol <- mex_swe[selected_cases, "V240"]
spol_skup <- as.data.table(cbind(spol, skupine))

# kontingenčna tabela spremenljivk spol in skupine
x <- addmargins(xtabs(~spol+skupine, data = spol_skup))

# tabela z deleži zgornje tabele
xx <- cbind(round(prop.table(x[,-3], 1)*100, 1),
            Sum = rep(100, nrow(x)))

# oblikovana kontingenčna tabela 
tabela3.3.6 <- as.data.table(rbind(x[1,], xx[1,], x[2,], xx[2,], x[3,], xx[3,]))
tabela3.3.6 <- cbind(spol = c("M", "M", "Ž", "Ž", "Total", "Total"), tabela3.3.6)
colnames(tabela3.3.6)[c(2:4)] <- c("Nizko", "Visoko", "Total")

# povezanost spola in skupin
tabela3.3.7 <- rbind(hi_hvadrat = c(chisq.test(x[-3,-3])$statistic, chisq.test(x[-3,-3])$p.value),
                     continuity_corr = c(prop.test(x[-3,-3], correct=TRUE)$statistic, prop.test(x[-3,-3], correct=TRUE)$p.value),
                     fisher = c(NA, fisher.test(x, simulate.p.value=TRUE)$p.value))

colnames(tabela3.3.7) <- c("vrednost", "p_vrednost")

# koeficienta Cramer V in kontingenčni koef. sta sumljivo nizka
tabela3.3.8 <- assocstats(x[-3,-3])







 









##### Povezanost usmerjenosti z družbenim razredom #####
izo <- mex_swe[selected_cases, "V248"]
izo_skup <- as.data.table(cbind(izo, skupine))

# kontingenčna tabela spremenljivk izo in skupine
x <- addmargins(xtabs(~izo+skupine, data = izo_skup))

# tabela z deleži zgornje tabele
xx <- cbind(round(prop.table(x[,-3], 1)*100, 1),
            Sum = rep(100, nrow(x)))

# definiramo list v katerem bodo podatki o numerusih in deležih
dat <- list()

# definiramo vektor z imeni (mora se 2x ponoviti za N in za %)
names <- c()
for (i in c(1:nrow(x))){ 
  dat[[i]] <- rbind(x[i, ], xx[i, ])
  names <- c(names, 
             paste(levels(as_factor(mex_swe[selected_cases, "V248"]))[i+5], " N"),
             paste(levels(as_factor(mex_swe[selected_cases, "V248"]))[i+5], " %"))
}

# na koncu vektorja damo podatke o Totalu
names[c(19:20)] <- c("Total N", "Total %")
tabela3.3.12 <- do.call(rbind, dat)

# podatkam dodamo imena
tabela3.3.12 <- as.data.table(cbind(izo=names, tabela3.3.12))
colnames(tabela3.3.12)[c(2:4)] <- c("Nizko", "Visoko", "Total")


# povezanost izoa in skupin
tabela3.3.13 <- rbind(hi_hvadrat = c(chisq.test(x[-10,-3])$statistic, chisq.test(x[-10,-3])$p.value),
                      continuity_corr = c(prop.test(x[-10,-3], correct=TRUE)$statistic, prop.test(x[-10,-3], correct=TRUE)$p.value),
                      fisher = c(NA, fisher.test(x, simulate.p.value=TRUE)$p.value))

colnames(tabela3.3.13) <- c("vrednost", "p_vrednost")

# koeficienta Cramer V in kontingenčni koef. sta sumljivo nizka
tabela3.3.14 <- assocstats(x[-10,-3])












