#rm(list = ls())

library(haven)
library(tidyr)
library(dplyr)
library(ggplot2)
library(psych)
library(questionr)
library(scales)
library(gridExtra)
library(data.table)
library(Weighted.Desc.Stat)
library(car)

# Load z read_sav (potrebujemo haven 1.1.0. (1.1.1. dela težave))
WV6 <- as.data.frame(read_sav("data/WV6_Data_spss_v_2016_01_01.sav"))

source("freq_q.R")

##### 1. del seminarske naloge #####
WV6_mex_swe <- WV6 %>%
  filter(V2 == 484 | WV6$V2 == 752)

# freq <- function(df, var, ime_var) {
#   df <- df[df$V2 == 484 | df$V2 == 752, ]
# 
#   table <- count_(df, as_factor(var), wt= "V258")
#   names(table)[1] <- ime_var
#   table$odstotek <- prop.table(table$n)*100
# 
#   if (is.na(sum(var))) {
#     table$veljaven_odstotek <- prop.table(table$n)*100
#   } else {
#     table$veljaven_odstotek <- c(prop.table(table(var, useNA = "no"))*100, NA)
#   }
# 
#   table$kumulativen_odstotek <- cumsum(table$veljaven_odstotek)
#   return(as.data.frame(adorn_totals(table)))
# }

## Država

tbl1.1 <- freq_q(droplevels(as_factor(WV6_mex_swe$V2)), digits = 1, cum = TRUE, total = TRUE)
tbl1.1$drzava <- row.names(tbl1.1)
tbl1.1 <- tbl1.1[, c(6, 1:5)]

slk1.1 <- ggplot(tbl1.1[-nrow(tbl1.1), ], aes(x ="", y = delez, fill = drzava)) +
                  geom_bar(width = 1, stat = "identity") +
                  coord_polar(theta = "y") +
                  geom_text(aes(label = paste0(round(delez, 1), "%")), position = position_stack(vjust = 0.5), size = 5) +
                  theme_minimal() +
                  theme(axis.text = element_blank(),
                        axis.ticks = element_blank(),
                        panel.grid = element_blank(),
                        axis.title.x =  element_blank(), 
                        axis.title.y = element_blank())

## Spol

tbl1.2 <- freq_q(droplevels(as_factor(WV6_mex_swe$V240)), digits = 1, cum = TRUE, total = TRUE)
tbl1.2$spol <- row.names(tbl1.2)
tbl1.2 <- tbl1.2[, c(6, 1:5)]

slk1.2 <- ggplot(tbl1.2[-nrow(tbl1.2), ], aes(x ="", y = delez, fill = spol)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar(theta = "y") +
  geom_text(aes(label = paste0(round(delez, 1), "%")), position = position_stack(vjust = 0.5), size = 5) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.title.x =  element_blank(), 
        axis.title.y = element_blank())

tbl1.2 <- questionr::freq(WV6_mex_swe$V240, digits = 1, cum = TRUE, total = TRUE)

## Izobrazba

tabela1.3.1 <- freq_q(WV6_mex_swe$V248, digits = 1, cum = TRUE, total = TRUE)

tbl1.3 <- as.data.frame(count(WV6_mex_swe, izo = as_factor(V248), wt = V258))


slika1.3 <- ggplot(tbl1.3[-c(1:3), ], aes(x = izo, y = n)) +
                    geom_bar(stat = "identity") +
                    coord_flip()


tabela1.3.2 <- summary(WV6_mex_swe$V248)

## Družbeni razred
WV6_mex_swe$V238_R <- car::Recode(WV6_mex_swe$V238, "1:2 = 1; 3 = 2; 4 = 3; 5 = 4; else = NA", as.factor = TRUE)

WV6_mex_swe$V238_R <- factor(WV6_mex_swe$V238_R, 
                              levels = c(1:4), 
                              labels = c("Višji in višji srednji razred", "Nižji srednji razred", "Delavski razred", "Spodnji razred"))

tablea1.4.1 <- freq_q(WV6_mex_swe$V238_R, digits = 1, cum = TRUE, total = TRUE)

as.data.frame(wtd.table(WV6_mex_swe$V238_R, na.show = TRUE, weights = WV6_mex_swe$V258))

Freq <- function(var, weight, digit = 1, name = "variabla") {
  # var = variabla
  # weight = utež
  # digit = število decimalk
  # name = ime variable, ki se prikaže v prvem stolpcu
  # exclude = katere missinge želimo 
  if (sum(class(var) %in% "labelled") > 0) {
    var[var < 0] <- NA
  }
  
  var <- droplevels(as_factor(var))
  
  n <- wtd.table(var, weights = weight, na.show = TRUE)
  x <- as.data.frame(round(n, 0))
  
  raw <- prop.table(wtd.table(var, weights = weight, na.show = TRUE))*100
  valid <- prop.table(as.data.frame(wtd.table(var, weights = weight))[2])*100
  
  y <- as.data.frame(round(as.vector(raw), digit))
  z <- as.data.frame(round(cumsum(as.vector(raw)), digit))
  a <- rbind(round(valid, digit), NA)
  b <- rbind(round(cumsum(valid), digit), NA)
  
  xx <- cbind(x, y, z, a, b)
  
  names(xx)[1] <- name
  names(xx)[2] <- "n"
  names(xx)[3] <- "delež"
  names(xx)[4] <- "kum_delež"
  names(xx)[5] <- "valid_delež"
  names(xx)[6] <- "valid_kum_delež"
    
  levels(xx[, 1]) <- c(levels(xx[, 1]), "Total")
  new_row <- nrow(xx)+1
  
  xx[new_row, 1] <- "Total"
  xx[new_row, 2] <- sum(n)
  xx[new_row, 3] <- sum(raw, na.rm = TRUE)
  xx[new_row, 4] <- 100
  xx[new_row, 5] <- sum(valid, na.rm = TRUE)
  xx[new_row, 6] <- 100
  
  return(xx)
}

Freq(var = WV6_mex_swe$V2, weight = WV6_mex_swe$V258, digit = 1, name = "država")

Freq(var = WV6_mex_swe$V248, weight = WV6_mex_swe$V258, digit = 1, name = "država")

tabela1.4.1 <- Freq(var = WV6_mex_swe$V238_R, weight = WV6_mex_swe$V258, digit = 1, name = "družbeni_razred")

tabela1.4.2 <- summary(as.numeric(WV6_mex_swe$V238_R))

tmp <- WV6_mex_swe %>%
          filter(!is.na(V238_R)) %>%
          count(V238_R, wt = V258) %>%
          as.data.frame()
sapply(tmp, class)


ggplot(tmp, aes(V238_R, n, fill = V238_R, label = round(n,0))) +
  geom_bar(stat = "identity") +
  geom_text(vjust = 2) +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title.x =  element_blank(), 
        axis.title.y = element_blank())
  

##### Starost #####

tabela1.5 <- psych::describe(WV6_mex_swe$V242)


ggplot(WV6_mex_swe, aes(V242, fill = cut(V242, 100), weight = as.numeric(V258))) +
  geom_histogram(show.legend = FALSE) +
  scale_fill_discrete(h = c(260, 320), c = 10, l = 50) +
  theme_minimal()



##### Schwartzove spremenljivke #####
schwartz <- select(WV6_mex_swe, V70:V79, -V74B, V258)
schwartz[schwartz < 0] <- NA


sd <- c()
mean <- c()
kurtosis <- c()
skewness <- c()
valid_n <- c()
missing_n <- c()
median <- c()
for (i in colnames(schwartz)) {
  a <- schwartz[complete.cases(schwartz[, i]), ]
  sd <- c(sd, w.sd(a[, i], a[, "V258"]))
  mean <- c(mean, w.mean(a[, i], a[, "V258"]))
  kurtosis <- c(kurtosis, w.kurtosis(a[, i], a[, "V258"]))
  skewness <- c(skewness, w.skewness(a[, i], a[, "V258"]))
  valid_n <- c(valid_n, sum(wtd.table(a[, i], a[, "V258"])))
  missing_n <- c(missing_n, nrow(schwartz) - sum(wtd.table(a[, i], weights = a[, "V258"])))
  median <- c(median, median(a[, i]))
}

sch_desc <- data.frame(sd = round(sd, 1), 
                       mean = round(mean, 2), 
                       kurtosis = round(kurtosis, 1), 
                       skewness = round(skewness, 1), 
                       valid_n = round(valid_n, 0), 
                       missing_n = round(missing_n, 0), 
                       median = median)
sch_desc[, "schwartzova_var"] <- names(schwartz)

sch_desc <- sch_desc[, c(8, 5, 6, 2, 7, 1, 3, 4)]

tabela1.6 <- sch_desc[-nrow(sch_desc), ]


## Pretvorimo v numeric zaradi ggplot2
schwartz <- lapply(schwartz, as.numeric)
schwartz <- as.data.frame(schwartz)

## Vseh 8 grafov na eni sliki
a <- list()

for (i in names(schwartz)[-11]) {
    a[[i]] <- ggplot(schwartz, aes_string(x = i, fill = as.factor(i), weight = "V258")) +
      geom_bar(na.rm = TRUE, show.legend = FALSE) +
      scale_x_discrete(limits = c(seq(1, 6, 1))) +
      theme_minimal()
}

slika1.6.1_10 <- grid.arrange(a[[1]], a[[2]], a[[3]], a[[4]], a[[5]], a[[6]], a[[7]], a[[8]], a[[9]], a[[10]], ncol = 2, nrow = 5)

## Aritmetična sredina schwartzovih spremenljivk glede na državo
schwartz_mex_swe <- select(WV6_mex_swe, V2, V70:V79, -V74B, V258)
schwartz_mex_swe <- as.data.table(schwartz_mex_swe)

cols <- paste0("V", 70:79)

tabela2.1 <- list()
for (i in cols) {
  tabela2.1[[i]] <- (schwartz_mex_swe[!is.na(get(i)), .(w.mean(get(i), V258)), by = .(V2)])
}

tabela2.1 <- as.data.table(tabela2.1)
tabela2.1 <- select(tabela2.1, V70.V2, contains(".V1"))


## t-test država
tabela2.2 <- list()

for (i in cols) {
  tabela2.2[[i]][paste0(i, "_t")] <- t.test(formula(paste0(i, "~ droplevels(as_factor(V2))")), data = schwartz_mex_swe)$statistic
  tabela2.2[[i]][paste0(i, "_p")] <- t.test(formula(paste0(i, "~ droplevels(as_factor(V2))")), data = schwartz_mex_swe)$p.value
}

tabela2.2 <- as.data.table(tabela2.2)


## povprečje scwartzovih spremenljivk glede na spremenljivko spol

schwartz_mex_swe <- select(WV6_mex_swe, V240, V70:V79, -V74B, V258)
schwartz_mex_swe <- as.data.table(schwartz_mex_swe)

cols <- paste0("V", 70:79)

tabela2.3 <- list()
for (i in cols) {
  tabela2.3[[i]] <- (schwartz_mex_swe[!is.na(get(i)), .(w.mean(get(i), V258)), by = .(V240)])
}

tabela2.3 <- as.data.table(tabela2.3)
tabela2.3 <- select(tabela2.3, V70.V240, contains(".V1"))
tabela2.3 <- tabela2.3[c(2,1), ]


## t-test država
tabela2.4 <- list()

for (i in cols) {
  tabela2.4[[i]][paste0(i, "_t")] <- t.test(formula(paste0(i, "~ droplevels(as_factor(V240))")), data = schwartz_mex_swe)$statistic
  tabela2.4[[i]][paste0(i, "_p")] <- t.test(formula(paste0(i, "~ droplevels(as_factor(V240))")), data = schwartz_mex_swe)$p.value
}

tabela2.4 <- as.data.table(tabela2.4)


## ANOVA
schwartz_mex_swe <- select(WV6_mex_swe, V238_R, V70:V79, -V74B, V258)
schwartz_mex_swe <- as.data.table(schwartz_mex_swe)


tabela2.5 <- list()
for (i in cols) {
  tabela2.5[[i]][paste0(i, "_mean")] <- schwartz_mex_swe[!is.na(get(i)), .(mean = w.mean(get(i), V258)), by = .(V238_R)][c(4,2,1,3,5) ,2]
  
  tabela2.5[[i]][paste0(i, "_N")] <- schwartz_mex_swe %>%
                                      filter(!is.na(get(i))) %>%
                                      count(V238_R, wt = V258) %>%
                                      select(n)
}


tabela2.5 <- melt(tabela2.5)
tabela2.5["druzbeni_razred"] <- rep(c(levels(schwartz_mex_swe$V238_R), "NA"), length.out = 100)

tabela2.5 <- separate(tabela2.5, L2, into = c("variabla", "statistika"), sep = "\\_") %>% select(-L1) %>% select(variabla, druzbeni_razred, statistika, value)
tabela2.5 <- tabela2.5 %>% spread(key = statistika, value = value) %>% filter(druzbeni_razred != "NA")

tabela2.5 <- tabela2.5 %>% arrange(variabla, desc(druzbeni_razred))


########## ANOVA ##########
## družbeni razred


tabela2.6 <- list()
for (i in cols) {
  tabela2.6[[i]][[paste0(i, "_Df")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  schwartz_mex_swe))[[1]][[1]]
  tabela2.6[[i]][[paste0(i, "_SumSq")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  schwartz_mex_swe))[[1]][[2]]
  tabela2.6[[i]][[paste0(i, "_MeanSq")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  schwartz_mex_swe))[[1]][[3]]
  tabela2.6[[i]][[paste0(i, "_F")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  schwartz_mex_swe))[[1]][[4]]
  tabela2.6[[i]][[paste0(i, "_p")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  schwartz_mex_swe))[[1]][[5]]
}


tabela2.6 <- melt(tabela2.6)
tabela2.6 <- separate(tabela2.6, L2, into = c("variabla", "statistika"), sep = "\\_") %>% select(-L1) %>% select(variabla, statistika, value)
tabela2.6["tmp"] <- rep(c(1,2), length.out = nrow(tabela2.6))



tabela2.6 <- tabela2.6 %>% unite(statistika, col = "statistika", statistika, tmp, sep = "_") %>% spread(key = statistika, value = value) %>%
  select(variabla, SumSq_1, SumSq_2, Df_1, Df_2, MeanSq_1, MeanSq_2, F_1, p_1)


names(tabela2.6) <- c("variabla", 
                      "SumSq_between_groups", "SumnSq_within_groups",
                      "df_between_groups", "df_within_groups", 
                      "MeanSq_between_groups", "MeanSq_within_groups", 
                       "F", "p")









## izobrazba

schwartz_mex_swe <- select(WV6_mex_swe, V248, V70:V79, -V74B, V258) %>% filter(V248 > 0)

tabela2.7 <- list()
for (i in cols) {
  tabela2.7[[i]][[paste0(i, "_Df")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  schwartz_mex_swe))[[1]][[1]]
  tabela2.7[[i]][[paste0(i, "_SumSq")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  schwartz_mex_swe))[[1]][[2]]
  tabela2.7[[i]][[paste0(i, "_MeanSq")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  schwartz_mex_swe))[[1]][[3]]
  tabela2.7[[i]][[paste0(i, "_F")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  schwartz_mex_swe))[[1]][[4]]
  tabela2.7[[i]][[paste0(i, "_p")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  schwartz_mex_swe))[[1]][[5]]
}

tabela2.7 <- melt(tabela2.7)
tabela2.7 <- separate(tabela2.7, L2, into = c("variabla", "statistika"), sep = "\\_") %>% select(-L1) %>% select(variabla, statistika, value)
tabela2.7["tmp"] <- rep(c(1,2), length.out = nrow(tabela2.7))

tabela2.7 <- tabela2.7 %>% unite(statistika, col = "statistika", statistika, tmp, sep = "_") %>% spread(key = statistika, value = value) %>%
  select(variabla, SumSq_1, SumSq_2, Df_1, Df_2, MeanSq_1, MeanSq_2, F_1, p_1)


names(tabela2.7) <- c("variabla", 
                      "SumSq_between_groups", "SumnSq_within_groups",
                      "df_between_groups", "df_within_groups", 
                      "MeanSq_between_groups", "MeanSq_within_groups", 
                      "F", "p")


########## Povezanost - korelacije ##########

schwartz_mex_swe <- select(WV6_mex_swe, V70:V79, -V74B, V242)
slika2.1 <- cor(schwartz_mex_swe, use = "pairwise.complete.obs")


## 
schwartz_mex_swe <- select(WV6_mex_swe, V70:V79, -V74B, V248, V238_R)
schwartz_mex_swe[, "V238_R"] <- as.numeric(schwartz_mex_swe[, "V238_R"]) 



corr_test <- corr.test(schwartz_mex_swe, use = "pairwise.complete.obs", method = "spearman")
tabela2.8 <- cbind(corr_test$r[-c(11, 12), c(11, 12)], corr_test$n[-c(11, 12), c(11, 12)], corr_test$p[-c(11, 12), c(11, 12)])

names <- rownames(tabela2.8)
tabela2.8 <- as.data.table(tabela2.8)
tabela2.8[, "var"] <- names
tabela2.8 <- tabela2.8[, c(7, 1:6)]

names(tabela2.8) <- c("var", "V248_corr_koef", "V238_R_corr_koeficient", "V248_n", "V238_R_n", "V248_p", "V238_R_p")









########## Likertove lestvice ##########

likert <- select(WV6_mex_swe, V70:V79, -V74B, V258) 
likert[likert < 0] <- NA

likert_scale <- likert %>% rowwise() %>% mutate(usmer_nase = mean(c(V70,V71,V73,V75,V76), na.rm = TRUE),
                                          usmer_druge = mean(c(V72,V74,V77,V78,V79), na.rm = TRUE)) %>% select(usmer_nase, usmer_druge, V258)


nase <- c("V70","V71","V73","V75","V76")
druge <- c("V72","V74","V77","V78","V79")

sd <- c()
mean <- c()
kurtosis <- c()
skewness <- c()
valid_n <- c(sum(a[!is.na(rowSums(a[, nase])),"V258"]), sum(a[!is.na(rowSums(a[, druge])),"V258"]))
missing_n <- c(nrow(likert) - valid_n[1], nrow(likert) - valid_n[2])
for (i in colnames(likert_scale)[-3]) {
  a <- likert_scale[!is.na(likert_scale[, i]), ]
  sd <- c(sd, w.sd(a[, i], a[, "V258"]))
  mean <- c(mean, w.mean(a[, i], a[, "V258"]))
  kurtosis <- c(kurtosis, w.kurtosis(a[, i], a[, "V258"]))
  skewness <- c(skewness, w.skewness(a[, i], a[, "V258"]))
}

tabela3.1 <- data.table(sd = round(sd, 1), 
                       mean = round(mean, 2), 
                       kurtosis = round(kurtosis, 1), 
                       skewness = round(skewness, 1), 
                       valid_n = round(valid_n, 0), 
                       missing_n = round(missing_n, 0))

slika3.1 <- ggplot(likert_scale, aes(usmer_nase, fill = usmer_nase, weight = as.numeric(V258))) +
            geom_histogram(binwidth = 0.2, show.legend = FALSE) +
            scale_fill_discrete(h = c(260, 320), c = 10, l = 50) +
            theme_minimal()
          
          
slika3.2 <- ggplot(likert_scale, aes(usmer_druge, fill = usmer_druge, weight = as.numeric(V258))) +
            geom_histogram(binwidth = 0.2, show.legend = FALSE) +
            scale_fill_discrete(h = c(260, 320), c = 10, l = 50) +
            theme_minimal()


##### Preverjanje domneve o razliki aritmetičnih sredin #####
# država opisne

likert <- cbind(likert_scale, V2 = WV6_mex_swe$V2)

a <- likert %>% filter(V2 == 484)
n <- c(nase_valid_n = sum(a[!is.na(a$usmer_nase), "V258"]), 
       druge_valid_n = sum(a[!is.na(a$usmer_druge), "V258"]))

mean <- c(nase_mean = w.mean(a[!is.na(a$usmer_nase), "usmer_nase"], a[!is.na(a$usmer_nase), "V258"]), 
          druge_mean = w.mean(a[!is.na(a$usmer_druge), "usmer_druge"], a[!is.na(a$usmer_druge), "V258"]))

tmp1 <- data.table(drzava = rep("Mexico", 2), var = c("usmer_nase", "usmer_druge"), n, mean)

a <- likert %>% filter(V2 == 752)
n <- c(nase_valid_n = sum(a[!is.na(a$usmer_nase), "V258"]), 
       druge_valid_n = sum(a[!is.na(a$usmer_druge), "V258"]))

mean <- c(nase_mean = w.mean(a[!is.na(a$usmer_nase), "usmer_nase"], a[!is.na(a$usmer_nase), "V258"]), 
          druge_mean = w.mean(a[!is.na(a$usmer_druge), "usmer_druge"], a[!is.na(a$usmer_druge), "V258"]))

tmp2 <- data.table(drzava = rep("Sweden", 2), var = c("usmer_nase", "usmer_druge"), n, mean)

tabela3.2 <- rbind(tmp1, tmp2)
tabela3.2 <- tabela3.2[c(1,3,2,4),]

## država t-test

likert <- cbind(likert_scale[,-3], V2 = WV6_mex_swe$V2)

enak_var <- var.test(likert$usmer_nase, likert$V2)

usmer_nase_test <- t.test(usmer_nase ~ droplevels(as_factor(V2)), likert)
usmer_druge_test <- t.test(usmer_druge ~ droplevels(as_factor(V2)), likert)


tabela3.3 <- data.table(drzava = c("Mexico", "Sweden"), 
               t = c(usmer_nase_test[[1]], usmer_druge_test[[1]]),
               df = c(usmer_nase_test[[2]], usmer_druge_test[[2]]),
               p = c(usmer_nase_test[[3]], usmer_druge_test[[3]]))


# spol opisne
likert <- cbind(likert_scale, V240 = WV6_mex_swe$V240)

a <- likert %>% filter(V240 == 1)
n <- c(nase_valid_n = sum(a[!is.na(a$usmer_nase), "V258"]), 
       druge_valid_n = sum(a[!is.na(a$usmer_druge), "V258"]))

mean <- c(nase_mean = w.mean(a[!is.na(a$usmer_nase), "usmer_nase"], a[!is.na(a$usmer_nase), "V258"]), 
          druge_mean = w.mean(a[!is.na(a$usmer_druge), "usmer_druge"], a[!is.na(a$usmer_druge), "V258"]))

tmp1 <- data.table(drzava = rep("Moški", 2), var = c("usmer_nase", "usmer_druge"), n, mean)

a <- likert %>% filter(V240 == 2)
n <- c(nase_valid_n = sum(a[!is.na(a$usmer_nase), "V258"]), 
       druge_valid_n = sum(a[!is.na(a$usmer_druge), "V258"]))

mean <- c(nase_mean = w.mean(a[!is.na(a$usmer_nase), "usmer_nase"], a[!is.na(a$usmer_nase), "V258"]), 
          druge_mean = w.mean(a[!is.na(a$usmer_druge), "usmer_druge"], a[!is.na(a$usmer_druge), "V258"]))

tmp2 <- data.table(drzava = rep("Ženski", 2), var = c("usmer_nase", "usmer_druge"), n, mean)

tabela3.4 <- rbind(tmp1, tmp2)
tabela3.4 <- tabela3.4[c(1,3,2,4),]

# spol t-test

likert <- cbind(likert_scale[,-3], V240 = WV6_mex_swe$V240)
enak_var <- var.test(likert$usmer_nase, likert$V240)

usmer_nase_test <- t.test(usmer_nase ~ droplevels(as_factor(V240)), likert)
usmer_druge_test <- t.test(usmer_druge ~ droplevels(as_factor(V240)), likert)


tabela3.5 <- data.table(drzava = c("Mexico", "Sweden"), 
               t = c(usmer_nase_test[[1]], usmer_druge_test[[1]]),
               df = c(usmer_nase_test[[2]], usmer_druge_test[[2]]),
               p = c(usmer_nase_test[[3]], usmer_druge_test[[3]]))



########## ANOVA likert ##########
# izobrazba

likert_anova <- cbind(likert_scale, V248 = WV6_mex_swe$V248)

tabela3.6 <- list()
for (i in c("usmer_nase", "usmer_druge")) {
  tabela3.6[[i]][[paste0(i, ".Df")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  likert_anova))[[1]][[1]]
  tabela3.6[[i]][[paste0(i, ".SumSq")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  likert_anova))[[1]][[2]]
  tabela3.6[[i]][[paste0(i, ".MeanSq")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  likert_anova))[[1]][[3]]
  tabela3.6[[i]][[paste0(i, ".F")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  likert_anova))[[1]][[4]]
  tabela3.6[[i]][[paste0(i, ".p")]] <- summary(aov(formula(paste0(i, " ~ V248")), data =  likert_anova))[[1]][[5]]
}


tabela3.6 <- melt(tabela3.6)
tabela3.6 <- separate(tabela3.6, L2, into = c("variabla", "statistika"), sep = "\\.") %>% select(-L1) %>% select(variabla, statistika, value)
tabela3.6["tmp"] <- rep(c(1,2), length.out = nrow(tabela3.6))



tabela3.6 <- tabela3.6 %>% unite(statistika, col = "statistika", statistika, tmp, sep = "_") %>% spread(key = statistika, value = value) %>%
  select(variabla, SumSq_1, SumSq_2, Df_1, Df_2, MeanSq_1, MeanSq_2, F_1, p_1)


names(tabela3.6) <- c("variabla", 
                      "SumSq_between_groups", "SumnSq_within_groups",
                      "df_between_groups", "df_within_groups", 
                      "MeanSq_between_groups", "MeanSq_within_groups", 
                      "F", "p")


# družbeni razred
likert_anova <- cbind(likert_scale, V238_R = WV6_mex_swe$V238_R)

tabela3.7 <- list()
for (i in c("usmer_nase", "usmer_druge")) {
  tabela3.7[[i]][[paste0(i, ".Df")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  likert_anova))[[1]][[1]]
  tabela3.7[[i]][[paste0(i, ".SumSq")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  likert_anova))[[1]][[2]]
  tabela3.7[[i]][[paste0(i, ".MeanSq")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  likert_anova))[[1]][[3]]
  tabela3.7[[i]][[paste0(i, ".F")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  likert_anova))[[1]][[4]]
  tabela3.7[[i]][[paste0(i, ".p")]] <- summary(aov(formula(paste0(i, " ~ V238_R")), data =  likert_anova))[[1]][[5]]
}


tabela3.7 <- melt(tabela3.7)
tabela3.7 <- separate(tabela3.7, L2, into = c("variabla", "statistika"), sep = "\\.") %>% select(-L1) %>% select(variabla, statistika, value)
tabela3.7["tmp"] <- rep(c(1,2), length.out = nrow(tabela3.7))



tabela3.7 <- tabela3.7 %>% unite(statistika, col = "statistika", statistika, tmp, sep = "_") %>% spread(key = statistika, value = value) %>%
  select(variabla, SumSq_1, SumSq_2, Df_1, Df_2, MeanSq_1, MeanSq_2, F_1, p_1)


names(tabela3.7) <- c("variabla", 
                      "SumSq_between_groups", "SumnSq_within_groups",
                      "df_between_groups", "df_within_groups", 
                      "MeanSq_between_groups", "MeanSq_within_groups", 
                      "F", "p")









# 
# a <- likert %>% filter(V2 == 484)
# valid_n <- c(sum(a[!is.na(rowSums(a[, nase])),"V258"]), sum(a[!is.na(rowSums(a[, druge])),"V258"]))
# for (i in colnames(likert_scale)[-3]) {
#   a <- likert_scale[!is.na(likert_scale[, i]), ]
#   sd <- c(sd, w.sd(a[, i], a[, "V258"]))
#   mean <- c(mean, w.mean(a[, i], a[, "V258"]))
#   kurtosis <- c(kurtosis, w.kurtosis(a[, i], a[, "V258"]))
#   skewness <- c(skewness, w.skewness(a[, i], a[, "V258"]))
# }







likert <- cbind(likert_scale, V2 = WV6_mex_swe$V2) 

slika3.3 <- ggplot(likert, aes(usmer_druge, usmer_nase, color = as_factor(V2), weight = as.numeric(V258))) +
            geom_jitter(width = 0.1, height = 0.1) +
            theme_minimal() +
            theme(legend.title=element_blank()) +
            xlim(0, 7) +
            ylim(0, 7)

