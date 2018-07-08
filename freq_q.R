freq_q <- function(x, digits = 1, cum = FALSE, total = FALSE, exclude = NULL, 
          sort = "", valid = !(NA %in% exclude), levels = c("prefixed", 
                                                            "labels", "values"), na.last = TRUE) 
{
  levels <- match.arg(levels)
  if (is.table(x)) 
    tab <- x
  else tab <- table(labelled::to_factor(x, levels), exclude = exclude)
  effectifs <- as.vector(tab)
  pourc <- as.vector(effectifs/sum(effectifs) * 100)
  result <- data.frame(n = effectifs, pourc = pourc)
  if (valid) {
    user_na <- unique(as.character(labelled::to_factor(x, 
                                                       levels)[is.na(x)]))
    NA.position <- which(is.na(names(tab)) | names(tab) %in% 
                           user_na)
    n.na <- sum(tab[NA.position])
    valid.pourc <- as.vector(effectifs/(sum(effectifs) - 
                                          n.na) * 100)
    valid.pourc[NA.position] <- 0
    result <- cbind(result, valid.pourc)
  }
  rownames(result) <- ifelse(is.na(names(tab)), "NA", names(tab))
  if (sort == "inc") 
    result <- result[order(result$n), ]
  if (sort == "dec") 
    result <- result[order(result$n, decreasing = TRUE), 
                     ]
  if (na.last && "NA" %in% rownames(result)) {
    result <- rbind(result[-which(rownames(result) == "NA"), 
                           ], result["NA", ])
  }
  if (total) 
    result <- rbind(result, Total = apply(result, 2, sum))
  if (total & valid) 
    result[length(result$pourc), "valid.pourc"] <- 100
  if (cum) {
    pourc.cum <- cumsum(result$pourc)
    if (total) 
      pourc.cum[length(pourc.cum)] <- 100
    result <- cbind(result, pourc.cum)
    if (valid) {
      valid.pourc.cum <- cumsum(result$valid.pourc)
      if (total) 
        valid.pourc.cum[length(valid.pourc.cum)] <- 100
      result <- cbind(result, valid.pourc.cum)
    }
  }
  if (valid) {
    NA.position <- which(rownames(result) == "NA" | rownames(result) %in% 
                           user_na)
    result[NA.position, "valid.pourc"] <- NA
    if (cum) 
      result[NA.position, "valid.pourc.cum"] <- NA
  }
  names(result)[names(result) == "pourc"] <- "delez"
  names(result)[names(result) == "valid.pourc"] <- "veljaven_delez"
  names(result)[names(result) == "pourc.cum"] <- "kumulativni_delez"
  names(result)[names(result) == "valid.pourc.cum"] <- "veljaven_kum_delez"
  class(result) <- c("freqtab", class(result))
  round(result, digits = digits)
}
