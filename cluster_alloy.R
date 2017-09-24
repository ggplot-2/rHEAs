cluster_alloy <- function(HEAs, Alloy_info) {
  
  temp <- HEAs %>%
    select(Alloy_Sys, Alloy, Structure) %>%
    rename(Element = Structure)
  temp[temp == ""] <- NA
  temp <- temp %>%
    na.omit() 
  temp <- temp %>%
    rbind(Alloy_info[1:3]) %>%
    tbl_df() 
  temp <- temp %>%
    group_by(Alloy_Sys, Alloy)
  temp$Element <- as.character(temp$Element)
  temp <- temp %>%
    count(Element, sort = TRUE)
  # temp <- distinct(temp)
  library(tidytext)
  tdm <- temp %>%
    cast_tdm(Alloy_Sys, Element, n)
  library(proxy)
  d <- dist(as.matrix(tdm), method= "cosine")
  hc <- hclust(d, method= "complete")
  pdf("./Figures/Alloy_Clustering.pdf",
      width = 16.53,
      height = 16.53)
  library(ape)
  f <- plot(as.phylo(hc), type = "fan", tip.color = hsv(runif(15, 0.65, 
                                                              0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20, 
                                                                                                                                                        0.5, 3), use.edge.length = TRUE, col = "gray80")
  dev.off()
  ID <- cutree(hc, k = 6)
  ID <- data.frame(names(ID), ID)
  ID <- ID %>%
    rename(Alloy_Sys = names.ID.)
  return(ID)
}
