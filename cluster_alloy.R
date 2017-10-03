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
  # pdf("./Figures/Alloy_Clustering.pdf",
  #     width = 16.53,
  #     height = 16.53)
  # library(ape)
  # f <- plot(as.phylo(hc), type = "fan", tip.color = hsv(runif(15, 0.65, 
  #                                                             0.95), 1, 1, 0.7), edge.color = hsv(runif(10, 0.65, 0.75), 1, 1, 0.7), edge.width = runif(20, 
  #                                                                                                                                                       0.5, 3), use.edge.length = TRUE, col = "gray80")
  # dev.off()
  pdf("./Figures/Alloy_Clustering.pdf",
      width = 16.53,
      height = 16.53)
  library(ape)
  mypal=c("#556270", "#1B676B", "#0e81fd", "#FF6B6B", "#C44D58", "#000000")
  ID <- cutree(hc, k = 6)
  op = par(bg="#E8DDCB")
  plot(as.phylo(hc), 
       type="fan", 
       tip.color = mypal[ID], 
       col = "red")
   dev.off()
   #########################################
   
   plot.fan <- function(hc, nclus = 6) {
     palette <- c("#E3170D",
                  "#00BFC4",
                  "#00BA38",
                  "#A020F0",
                  "#FF6100",
                  "#0000FF")[1:nclus]

     
     clus <- cutree(hc,nclus)
     X <- as.phylo(hc)
     edge.clus <- sapply(1:nclus,function(i)max(which(X$edge[,2] %in% which(clus==i))))
     order     <- order(edge.clus)
     edge.clus <- c(min(edge.clus),diff(sort(edge.clus)))
     edge.clus <- rep(order,edge.clus)
     op = par(bg="#E8DDCB")
     plot(X,type='fan',
          tip.color=palette[clus],
          edge.color=palette[edge.clus],
          # label.offset=0.002,
          no.margin=TRUE,
          cex = 1.1)  
   }
   plot.fan(hc, 6)
   tiff("./Figures/Alloy_Clustering.tif",
        width = 1246.18,
        height = 1256.07,
        compression = "none",
        type = "cairo")
   plot.fan(hc, 6)
   dev.off()
   # mypal=c("#556270", "#1B676B", "#0e81fd", "#FF6B6B", "#C44D58", "#000000")
   # mypal = c("#A020F0", 
   #           "#00BFC4", 
   #           "#00BA38", 
   #           "#E3170D", 
   #           "#FF6100", 
   #           "#0000FF")
   # op = par(bg="#E8DDCB")
   # 
   # plot(as.phylo(hc), 
   #      type="fan", 
   #      tip.color = mypal[ID],
   #      edge.color = mypal[ID],
   #      show.node.label = TRUE,
   #      cex = 1)
   # dev.off()

  

  
  ID <- data.frame(names(ID), ID)
  ID <- ID %>%
    rename(Alloy_Sys = names.ID.)
  return(ID)
}
