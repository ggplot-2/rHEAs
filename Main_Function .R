rm(list = ls())
#### Import HEAs Dataset ####
library(xlsx)
library(stringr)
library(dplyr)
library(ggplot2)

Rename2system <- function(alloy){
  
  # Rename the alloy with a sep '-'
  
  library(stringr)
  library(dplyr)
  
  alloy <- alloy %>%
    str_replace_all("([A-Z])", "\\-\\1") %>%
    str_replace_all("-+", "-") %>%
    str_replace("-+", "")
  
  return(alloy)
  
}
Rename2system2 <- function(alloy){
  # Remove the digits in alloy titles
  library(stringr)
  library(dplyr)
  
  alloy <- alloy %>%
    str_replace_all("([0-9|.|x|(|)]+)", "") 
  
  return(alloy)
  
}

HEAs <- read.xlsx2("./Data/High-entropy alloy system.xlsx",
                   sheetName = "Main",
                   encoding = "UTF-8") %>%
                   select(-contains("X.")) %>%
                   mutate(Alloy = Rename2system(Alloy))  %>%
                   mutate(Alloy_Sys = Rename2system2(Alloy))  %>%
                   select(Alloy_Sys, everything()) %>%
                   distinct()

##### Clean data and get the neat BigMatrix #####
ExtractAlloy <- function(alloy){
  
  periodic <- read.table("./Data/periodic-table.csv",
                         header = TRUE,
                         sep = ",")
  library(stringr)
  library(dplyr)
  alloy_old <- alloy
  Alloy <- c(alloy_old)
  alloy <- alloy %>%
    str_replace_all("(\\s)+", "")
  alloy <- alloy %>%
    str_replace_all("(\\D-)", "\\11-")
  alloy <- alloy %>%
    str_replace_all("(\\s)+", "")
  alloy <- alloy %>%
    str_replace_all("(\\D)$", "\\1-1")
  alloy <- alloy %>%
    str_replace_all("x", "")
  alloy <-  as.vector(alloy[[1]])
  N <- length(alloy)
  for(i in 1:N) {
    alloy[i] <- str_replace_all(alloy[i], "(\\d+)", "-\\1")
  }
  alloy <- alloy %>%
    str_replace_all("\\.-", ".")
  alloy <- alloy %>%
    str_replace_all("-+", "-")
  N <- length(alloy)
  for(i in 1:N) {
    alloy[i] <- str_split(alloy[i], "-")
    Alloy[i] <- alloy_old
  }
  
  alloy <- as.data.frame(matrix(unlist(alloy), ncol = 2, byrow = T))
  
  alloy <- alloy %>%
    rename(Element = V1, Weight = V2)
  
  Element <- factor(as.vector(unlist(alloy[1])),
                    periodic$atomic_symbol)
  Weight <- as.numeric(as.vector(unlist(alloy[2])))
  
  df <- data.frame(Alloy, Element, Weight)
  
  df <- df %>%
    mutate(Weight_atom = Weight/sum(Weight)) %>%
    select(everything(), Weight_atom)
  df <- df %>%
    mutate(Alloy_Sys = rename2system2(Alloy)) %>%
    select(Alloy_Sys, everything())
  
  df
  
  
}
GenerateAlloyInfo <- function(HEAs) {
  r <- dim(HEAs)[1]
  
  Alloy_info <- ExtractAlloy(HEAs[1, 2])
  
  for( i in 2:r) {
    Alloy_info <- Alloy_info %>%
      rbind(ExtractAlloy(HEAs[i, 2]))
  }
  
  Alloy_info <- Alloy_info %>%
    distinct()
  
  return(Alloy_info)
  
}

alloyInfo <- GenerateAlloyInfo(HEAs)

GnerateBigMatrix <- function(alloyInfo, HEAs){
  
  # Generate a neat dataframe of HEAs
  
  library(reshape2)
  
  md <- melt(Alloy_info, id = c("Alloy_Sys", "Alloy", "Element", "Weight"))
  element_matrix <- dcast(md, Alloy~Element)
  element_matrix[is.na(element_matrix)] <- 0
  
  BigMatrix <- inner_join(HEAs, element_matrix, by = "Alloy")
  BigMatrix$Alloy_Sys <- BigMatrix$Alloy_Sys %>%
    as.factor()
  
  BigMatrix$Alloy <- BigMatrix$Alloy %>%
    as.factor()
  
  BigMatrix$Structure[BigMatrix$Structure == ""] <- NA
  
  BigMatrix$Density <- BigMatrix$Density %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$T <- BigMatrix$T %>%
    as.character() %>%
    as.numeric() %>%
    round()
  
  BigMatrix$T[is.na(BigMatrix$T)] <- 25
  
  BigMatrix$E <- BigMatrix$E %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Yield_Strength <- BigMatrix$Yield_Strength %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Yield_Strain <- BigMatrix$Yield_Strain %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Peak_Stress	<- BigMatrix$Peak_Stress %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Peak_Strain <- BigMatrix$Peak_Strain %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Fracture_Strength <- BigMatrix$Fracture_Strength %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Fracture_Strain <- BigMatrix$Fracture_Strain %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Compression_Yield_Strength <- BigMatrix$Compression_Yield_Strength %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Compression_Strain <- BigMatrix$Compression_Strain %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Strain <- BigMatrix$Strain %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Vickers_hardness. <- BigMatrix$Vickers_hardness. %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix$Wear_Resistance <- BigMatrix$Wear_Resistance %>%
    as.character() %>%
    as.numeric()
  
  BigMatrix <- BigMatrix %>%
    group_by(Alloy_Sys, Alloy)
  
  return(BigMatrix)
  
}
BigMatrix <- GnerateBigMatrix(alloyInfo, HEAs)
         # Alloy_info <- generate_Alloy_info(HEAs)

##### Generate alloy clustering map ######
ClusterAlloy <- function(HEAs, alloyInfo) {
  # Calculate the similarity of different HEAs
  # and generate a clustering map.
  library(dplyr)
  
  temp <- HEAs %>%
    select(Alloy_Sys, Alloy, Structure) %>%
    rename(Element = Structure)
  temp[temp == ""] <- NA
  temp <- temp %>%
    na.omit() 
  temp <- temp %>%
    rbind(alloyInfo[1:3]) %>%
    tbl_df() 
  temp <- temp %>%
    group_by(Alloy_Sys, Alloy)
  temp$Element <- as.character(temp$Element)
  temp <- temp %>%
    count(Element, sort = TRUE)
  
  
  library(tidytext)
  tdm <- temp %>%
    cast_tdm(Alloy_Sys, Element, n)
  
  library(proxy)
  d <- dist(as.matrix(tdm), method= "cosine")
  hc <- hclust(d, method= "complete")
  
  # Draw the clustering map 
  
  plot.fan <- function(hc, nclus = 6) {
    
    palette <- c("#00BA38",
                 "#E3170D",
                 "#0000FF",
                 "#FF00FF",
                 "#A020F0",
                 "#00BFC4")[1:nclus]
    
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
  
  tiff("./Figures/alloyClustering.tif",
       width = 6000,
       height = 6000,
       compression = "none",
       type = "cairo",
       res = 500)
  plot.fan(hc, 6)
  dev.off()
  
  ID <- data.frame(names(ID), ID)
  ID <- ID %>%
    rename(Alloy_Sys = names.ID.)
  return(ID)
}
ID <- cluster_alloy(HEAs, alloyInfo)

# Add clustering ID to the BigMatrix
BigMatrix <- BigMatrix %>%
  inner_join(ID, by = "Alloy_Sys") 
BigMatrix <- BigMatrix %>%
  select(ID, everything())

##### Term Frequency analysis
# source('./element_fre_cal.R')
# source('./get_element_names.R')

CalElementFre <- function(alloyInfo) {
  # 计算元素词频，输出词频云图
  # 输出element frequency.csv
  
  library(wordcloud)
  library(scales)
  library(ggplot2)
  
  temp <-   alloyInfo %>%
    count(Element, sort = TRUE)
  
  # Element coloud 
  pdf("./Figures/Element/elementCloud.pdf",
      paper = "a4r")
  alloyInfo %>%
    count(Element, sort = TRUE) %>%
    with(wordcloud(Element, n, random.order = FALSE, scale = c(5, 0.5),
                   colors = c('red','blue','green',  'black', 'purple'),
                   rot.per = 0,
                   ordered.colors = FALSE))
  dev.off()
  
  
  
  elementFrequency <- alloyInfo %>%
    count(Element, sort = TRUE) %>%
    mutate(other = n / sum(n)) %>%
    ungroup()
  elementFrequency %>%
    rename(Symbol = Element, Fre = other) %>%
    select(1, 3) %>%
    write.csv(file = "./Data/elementFre.csv")
}
CalElementFre(Alloy_info)
element_fre_cal(Alloy_info)

# Plot element frequency distribution in periodic
source('./Source/PlotElementFrePeriodic.R', 
       encoding = 'UTF-8')
PlotElementFrePeriodic() 

##### PlotRelationMapGroup ####
PlotRelationMapGroup <- function(BigMatrix){
  library(latex2exp)

  ##### Phase Structure distribution ######
  ggplot(BigMatrix, aes(x = reorder(Structure,
                                    table(Structure)[Structure]))) +
    geom_bar(data = subset(BigMatrix, !is.na(Structure)), aes(fill = factor(ID))) +
    coord_flip() +
    scale_fill_discrete(name = "Alloy ID") +
    labs(x = "Structure")
  ggsave("./Figures/RelationMapGroup/Structure_distribution.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")

  ##### Alloy-Hardness distribution ####

  ggplot(BigMatrix, aes(x = Alloy, y = Vickers_hardness., color = T)) +
    geom_jitter(aes(shape = factor(ID))) +
    facet_wrap(~Structure) +
    theme(axis.text.x = element_blank()) +
    scale_colour_gradientn(colours = c("blue", "red", "orange"),
                           values = c(0, 1, 1),
                           name = expression("T/"~degree*C)) +
    scale_shape_discrete(name = "Alloy ID") +
    labs(x = "High-Entropy Alloy",
         y = TeX("Vickers Hardness/kg$\\cdot$mm^{-2}"))
  ggsave("./Figures/RelationMapGroup/Alloy_hardness.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")

  ##### Hardness-Strength ####
  ggplot(BigMatrix, aes(x = Vickers_hardness., y = Compression_Yield_Strength,
                        color = T ,
                        shape = factor(ID))) +
    geom_point() +
    scale_colour_gradientn(colours = c("blue", "red", "orange"),
                           values = c(0, 1, 1),
                           name = expression("T/"~degree*C)) +
    labs(x = TeX("Vickers hardness/kg$\\cdot$mm^{-2}"),
         y = "Compression Yield Strength/MPa") +
    scale_shape_discrete(name = "Alloy ID") +
    geom_smooth()

  BigMatrix %>%
    # ungroup() %>%
    # filter(T<=25) %>%
    ggplot(aes(x = Vickers_hardness., y = Yield_Strength,
               group = ID + T,
               color = T,
               shape = factor(ID))) +
    geom_point() +
    geom_smooth(method = "lm",
                na.rm = TRUE)+
    scale_shape_discrete(name = "Alloy ID") +
    scale_colour_gradientn(colours = c("blue", "red", "orange"),
                           values = c(0, 1, 1),
                           name = expression("T/"~degree*C)) +
    labs(x = TeX("Vickers hardness/kg$\\cdot$mm^{-2}"),
         y = "Yield strength/MPa")

  ggsave("./Figures/RelationMapGroup/Hardness_YieldStrength.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")

  ##### Hardness-Wear resistance #####
  ggplot(BigMatrix, aes(x = Wear_Resistance, y = Vickers_hardness.,
                        group = interaction(T, ID),
                        color = T ,
                        shape = factor(ID))) +
    geom_point() +
    stat_smooth(na.rm = TRUE) +
    scale_shape_discrete(name = "Alloy ID") +
    scale_colour_gradientn(colours = c("blue", "red", "orange"),
                           values = c(0, 1, 1),
                           name = expression("T/"~degree*C)) +
    labs(x = TeX("Wear resistance/m$\\cdot$mm^{-3}"),
         y = TeX("Vickers hardness/kg$\\cdot$mm^{-2}"))
  ggsave("./Figures/RelationMapGroup/Wear resistance_hardness.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")

  #### Rank performance ####
  # Hardness
  BigMatrix %>%
    ungroup() %>%
    top_n(10, Vickers_hardness.) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Vickers_hardness.,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y = Vickers_hardness.,
               fill = Structure)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = round(Vickers_hardness.)),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = TeX("Vickers hardness/kg$\\cdot$mm^{-2}")) +
    facet_wrap(~State, ncol = 1,
               labeller = label_both)
  ggsave("./Figures/RelationMapGroup/Ranks_Vickers_hardness.pdf",
         device = cairo_pdf,
         height = 297,
         width = 297,
         units = "mm")
  # Wearistance
  BigMatrix %>%
    ungroup() %>%
    top_n(8, Wear_Resistance) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Wear_Resistance,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y = Wear_Resistance,
               fill = Structure)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = round(Wear_Resistance)),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = TeX("Wear resistance/m$\\cdot$mm^{-3}"))
  ggsave("./Figures/RelationMapGroup/Ranks_Wear_Resistance.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
  # Young's modulus of elasticity
  BigMatrix %>%
    ungroup() %>%
    top_n(10, E) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           E,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y = E,
               fill = Structure)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = round(E)),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = "E/GPa")
  ggsave("./Figures/RelationMapGroup/Ranks_Young's modulus of elasticity.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
  # Yield Strength
  BigMatrix %>%
    ungroup() %>%
    top_n(10, Yield_Strength) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Yield_Strength,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y = Yield_Strength,
               fill = Structure)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = round(Yield_Strength)),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = "Yield strength/MPa")
  ggsave("./Figures/RelationMapGroup/Ranks_Yield_Strength.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
  # Peak Stress
  BigMatrix %>%
    ungroup() %>%
    top_n(10, Peak_Stress) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Peak_Stress,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y = Peak_Stress,
               fill = Alloy)) +
    geom_col(width = 0.5) +
    coord_flip() +
    facet_wrap(~T,
               ncol = 1,
               labeller = label_both) +
    geom_text(aes(label = round(Peak_Stress),
                  color = Alloy),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = "Peak strength/MPa") +
    theme(legend.position = "none")
  ggsave("./Figures/RelationMapGroup/Ranks_Peak_Strengt.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
  # Fracture_Strength
  BigMatrix %>%
    ungroup() %>%
    top_n(10, Fracture_Strength) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Fracture_Strength,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y = Fracture_Strength,
               fill = Structure)) +
    geom_col(aes(color = Alloy)) +
    coord_flip() +
    geom_text(aes(label = round(Fracture_Strength)),
              check_overlap = TRUE) +
    labs(y = "Fracture strength/MPa") +
    facet_grid(T ~ State,
               labeller = label_both)

  ggsave("./Figures/RelationMapGroup/Ranks_Fracture_Strength.pdf",
         device = cairo_pdf,
         height = 297,
         width = 420,
         units = "mm")
  # Compression_Yield_Strength
  BigMatrix %>%
    ungroup() %>%
    top_n(10,  Compression_Yield_Strength) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Compression_Yield_Strength,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y =  Compression_Yield_Strength,
               fill = Structure)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = round(Compression_Yield_Strength)),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = " Compression yield strength/MPa")

  ggsave("./Figures/RelationMapGroup/Ranks_Compression_Yield_Strength.pdf",
         device = cairo_pdf,
         height = 210,
         width = 320,
         units = "mm")
  # Compression_Strain
  BigMatrix %>%
    ungroup() %>%
    top_n(10,  Fracture_Strain) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Fracture_Strain,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y =  Fracture_Strain*100,
               fill = Structure)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = Fracture_Strain*100),
              check_overlap = TRUE,
              hjust = -0.1) +
    labs(y = " Compression strain/%") +
    facet_grid(T~.,
               labeller = label_both)

  ggsave("./Figures/RelationMapGroup/Ranks_Fracture_Strain.pdf",
         device = cairo_pdf,
         height = 297,
         width = 420,
         units = "mm")

  BigMatrix %>%
    ungroup() %>%
    top_n(10,  Peak_Strain) %>%
    distinct() %>%
    mutate(Alloy = reorder(Alloy,
                           Peak_Strain,
                           order = TRUE)) %>%
    ggplot(aes(x = Alloy,
               y =  Peak_Strain,
               color = Structure, fill = Alloy)) +
    geom_col() +
    coord_flip() +
    geom_text(aes(label = Peak_Strain),
              check_overlap = TRUE,
              hjust = -0.2) +
    labs(y = " Peak strain/%") +
    facet_grid(T~.,
               labeller = label_both)

  ggsave("./Figures/RelationMapGroup/Ranks_Peak_Strain.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
}
PlotRelationMapGroup(BigMatrix)

#### Random forest & ANN for Hardness  ####
PredictHardness <- function(BigMatrix) {
  # Predict hardness
  
  library(dplyr)
  ##### Scale data #####
  norm_data <- BigMatrix %>%
    ungroup()
  norm_data[8] <- scale(BigMatrix[8])
  scale_info <- data.frame(center = attr(scale(BigMatrix[, c(8)]),'scaled:center'),
                           scale = attr(scale(BigMatrix[, c(8)]),'scaled:scale'))
  for(i in 9:19) {
    norm_data[i] <- scale(BigMatrix[i])
    scale_info[i-7, ] <- data.frame(center = attr(scale(BigMatrix[, c(i)]),'scaled:center'),
                                    scale = attr(scale(BigMatrix[, c(i)]),'scaled:scale'))
  }
  
  norm_data <- norm_data %>%
    rename(Hardness = Vickers_hardness.) %>%
    filter(Hardness != " ")
  
  ###### Seperate data #####
  library(caret)
  set.seed(42)
  Index <- createDataPartition(norm_data$Hardness, p = .8,
                               list = FALSE,
                               times = 1)
  train_data <- norm_data[Index, ]
  test_data <- norm_data[-Index, ]
  formula <- names(norm_data[23:58]) %>%
    paste(collapse = "+")
  formula <- paste("Hardness ~", formula, sep = " ") %>%
    as.formula()
  
  ##### Random forest for Hardness ######
  library(randomForest)
  set.seed(43)
  rf <- randomForest(formula,
                     data = train_data,
                     mtry = 27,
                     importance = TRUE, 
                     proximity = TRUE,
                     keep.forest = TRUE)
  
  set.seed(42)
  # cairo_pdf("./Figures/Hardness_prediction/RFTune_OOBError.pdf")
  jpeg("./Figures/Hardness_prediction/RFTune_OOBError.jpg")
  bestmtry <- tuneRF(norm_data[23:58],
                     norm_data$Hardness,
                     stepFactor=1.5, 
                     improve=1e-5, 
                     ntree = 300,
                     plot = TRUE,
                     doBest = TRUE,
                     trace = TRUE,
                     oob.prox = TRUE)
  dev.off()
  
  set.seed(43)
  rf <- randomForest(formula,
                     data = train_data,
                     ntree = 300,
                     mtry = 18,
                     importance = TRUE, 
                     proximity = TRUE,
                     keep.forest = TRUE)
  # cairo_pdf("./Figures/Hardness_prediction/RF_Practical.pdf")
  jpeg("./Figures/Hardness_prediction/RF_Practical.jpg")
  plot(rf,
       main = "Random forest for hardness prediction")
  dev.off()
  
  
  ### 计算模型变量重要性 ###
  im <- importance(rf,type=1)
  # cairo_pdf("./Figures/Hardness_prediction/varImpPlot.pdf")
  jpeg("./Figures/Hardness_prediction/varImpPlot_Hardness.jpg")
  varImpPlot(rf,
             main = "Element importance for hardness calculated with random forest")
  dev.off()
  tiff("./Figures/Hardness_prediction/varImpPlot_Hardness.tiff")
  varImpPlot(rf,
             main = "Element importance for hardness calculated with random forest")
  dev.off()
  
  #####预测集中预测数据与实际数据的比对######
  pred_rf <- predict(rf, test_data)
  pred_rf_unscale <- pred_rf*scale_info$scale[11] +scale_info$center[11]
  test_hard_unscale <- test_data$Hardness*scale_info$scale[11] +scale_info$center[11]
  library(miscTools)
  R2 <- rSquared(test_data$Hardness,
                 test_data$Hardness - pred_rf)
  rmse_scale <- RMSE(pred_rf, test_data$Hardness)
  rmse_unscale <- RMSE( pred_rf_unscale, test_hard_unscale)
  
  ###### BP-Neural network for hardness prediction #####
  # Visualization
  pdf("./Figures/Hardness_prediction/BPNNet_featurePlot_Hardness.pdf",
      height = 46.8,
      width = 33.1)
  featurePlot(x = norm_data[, 23:58],
              y = norm_data$Hardness)
  dev.off()
  # BPNet model
  # ##### BPNet ####
  library(neuralnet)
  set.seed(42)
  BPNNet <- neuralnet(formula,
                      data = train_data,
                      hidden = 7,
                      # rep = 15,
                      threshold = 0.03,
                      learningrate = 0.01,
                      algorithm = "rprop+",
                      err.fct = "sse",
                      act.fct = "tanh",
                      linear.output = FALSE)
  pdf("./Figures/Hardness_prediction/BPNNet.pdf")
  plot(BPNNet,
       rep = "best")
  dev.off()
  pred_BP_scale <- compute(BPNNet, test_data[, 23:58])$net.result
  pred_BP_unscale <- pred_BP_scale*scale_info$scale[11] + scale_info$center[11]
  R2_BP <- rSquared(test_data$Hardness,
                    test_data$Hardness - pred_BP_scale)
  rmse_BP_unscale <- RMSE( pred_BP_unscale, test_hard_unscale)
  # 
  library(ggplot2)
  library(latex2exp)
  
  ggplot(data = data.frame(Alloy_System = test_data$Alloy_Sys,
                           actual = test_hard_unscale,
                           pred_rf = pred_rf_unscale,
                           pred_BP = pred_BP_unscale), na.rm = TRUE) +
    geom_point( aes(x = actual,
                    y = pred_rf,
                    shape = Alloy_System,
                    color = "blue"),
                alpha = 0.7,
                size = 7) +
    geom_point(aes(x = actual,
                   y = pred_BP,
                   shape = Alloy_System,
                   color = "red"),
               size = 7,
               alpha = 0.9) +
    xlim(200, 800) +
    ylim(200, 800) +
    geom_abline(color = "cyan",
                size = 0.8) +
    guides(shape = guide_legend(ncol = 2)) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(size = 0.8),
          panel.border = element_rect(size = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.text.align = 0,
          legend.background =  element_rect(fill = "transparent",colour = NA),
          legend.position = c(.78,.22)) +
    labs(x = TeX("Actual vickers hardness/kg$\\cdot$mm^{-2}"),
         y = TeX("Predicted vickers hardness/kg$\\cdot$mm^{-2}"),
         shape = "Alloy system",
         color = "Machine learning model") + 
    scale_color_manual(values = c("blue", "red"),
                       labels = c(expression(paste("RF, ","R"^2," = 0.58, ","RMSE = 119.55.")),
                                  expression(paste("BPNN, ", "R"^2," = 0.65, ","RMSE = 108.85.")))) +
    scale_shape_manual(values = c(6:18, 0:1, 3)) +
    geom_text(aes(x = 330, y = 800,
                  label = "Machine learning model"),
              color = "black",
              size = 8) +
    geom_text(aes(x = 330, y = 770,
                  label = paste("BPNN: R^2 ==", round(R2_BP, 2), 
                                "~~", "RMSE == ",round(rmse_BP_unscale, 2), 
                                sep = " ",
                                collapse = ",")),
              color = "red",
              parse = TRUE,
              size = 4) +
    guides(color = FALSE) +
    geom_text(aes(x = 330, y = 740,
                  label = paste("RF: R^2 == ", round(R2, 2),
                                "~~", "RMSE == ",round(rmse_unscale, 2),
                                "\t\n",
                                sep = "",
                                collapse = ",")),
              color = "blue",
              parse = TRUE,
              size = 4) 
  
  
  ggsave("./Figures/Hardness_prediction/Hardness for Real-Predict.pdf",
         device = cairo_pdf,
         width = 11.69,
         height = 9)
  
  ggsave("./Figures/Hardness_prediction/Hardness forReal_Predict.tiff",
         width = 13,
         height = 8.034,
         dpi = 100)
  ### BPNN 预测评价lonely ####
  
  tiff("./Figures/Hardness_prediction/HardnessrealPredictNeat.tiff",
       width = 6000,
       height = 4000,
       compression = "none",
       type = "cairo",
       res = 410,
       antialias = "subpixel")
  library(ggrepel)
  ggplot(data = data.frame(Alloy_System = test_data$Alloy_Sys,
                           actual = test_hard_unscale,
                           pred_BP = pred_BP_unscale), 
         na.rm = TRUE) +
    geom_abline(slope = 1,
                intercept = 0,
                color = "blue",
                size = 0.8) +
    geom_point(aes(x = actual,
                   y = pred_BP),
               color = "red",
               size = 7) +
    labs(x = TeX("Actual vickers hardness/kg$\\cdot$mm^{-2}"),
         y = TeX("Predicted vickers hardness/kg$\\cdot$mm^{-2}")) +
    geom_label_repel(aes(x = round(actual),
                         y = round(pred_BP),
                         label = Alloy_System),
                     fill = "blue",
                     alpha = 0.9,
                     color = "white",
                     box.padding = 0.8,
                     point.padding = 0.4,
                     segment.color = "purple",
                     segment.size = 1,
                     segment.alpha = 0.7,
                     direction = "y",
                     force = 9,
                     nudge_y = 1e-5) +
    xlim(200, 800) +
    ylim(200, 800) +
    geom_text(aes(x = 250, 
                  y = 770,
                  label = "BPNN"),
              color = "black",
              parse = TRUE,
              size = 6) +
    geom_text(aes(x = 250, 
                  y = 740,
                  label = paste("R^2 ==", round(R2_BP, 2))),
              color = "black",
              parse = TRUE,
              size = 4) +
    geom_text(aes(x = 250, 
                  y = 720,
                  label = paste("RMSE == ",round(rmse_BP_unscale, 2))),
              color = "black",
              parse = TRUE,
              size = 4) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          panel.border = element_rect(size = 1.5),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 12,
                                   color = "black"),
          axis.line = element_line(size = 1),
          
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 12),
          legend.text.align = 0,
          legend.background =  element_rect(fill = "transparent",
                                            colour = NA),
          legend.position = c(.78,.22)) 
  dev.off()
  
  ###ggplot2 varImportance map
  var_importance <- data_frame(variable= rownames(importance(rf)),
                               importance = as.vector(importance(rf, type = 1)))
  var_importance <- arrange(var_importance, importance)
  var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
  write.csv(var_importance,
            file = "./Data/varIm_hard.csv")
  
}
PredictHardness(BigMatrix)

##### Random forest for phase structure #####
PredictStructure <- function(BigMatrix) {
  library(dplyr)
  norm_data <- BigMatrix %>%
    ungroup()
  for(i in 8:19) {
    norm_data[, c(i)] <- scale(norm_data[, c(i)])
  }
  norm_data <- norm_data %>%
    rename(Hardness = Vickers_hardness.) %>%
    filter(Structure != "")
  norm_data$Structure <- factor(norm_data$Structure)
  
  library(caret)
  set.seed(42)
  Index <- createDataPartition(norm_data$Structure, p = .8,
                               list = FALSE,
                               times = 1)
  train_data <- norm_data[Index, ]
  test_data <- norm_data[-Index, ]
  formula <- names(norm_data[23:58]) %>%
    paste(collapse = "+")
  formula <- paste("Structure ~", formula, sep = " ") %>%
    as.formula()
  
  ##### Random forest for Structure ######
  library(randomForest)
  ###### Tune using algorithm tools ######
  set.seed(42)
  library(caret)
  ###主函数中不运行
  control <- trainControl(method = "repeatedcv",
                          number = 5,
                          repeats = 3,
                          search = "grid")
  set.seed(42)
  tunegrid <- expand.grid(.mtry=c(1:50))
  rf_gridsearch <- train(formula,
                         data=train_data,
                         method="rf",
                         metric = "metric",
                         tuneGrid=tunegrid,
                         trControl=control)
  print(rf_gridsearch)
  cairo_pdf("./Figures/Structure_randomForest/RF_Phase_tuning.pdf")
  plot(rf_gridsearch)
  dev.off()
  
  ##### 建立随机森林模型 #######
  rf <- randomForest(formula,
                     data = train_data,
                     ntree = 1000,
                     mtry = 5,
                     importance = TRUE, 
                     proximity = TRUE,
                     keep.forest = TRUE,
                     sampsize=200)
  cairo_pdf("./Figures/Structure_randomForest/RF_Phase_After_tuning.pdf")
  plot(rf, 
       main = "Random forest for phase structure predication")
  dev.off()
  
  ### 计算模型变量重要性 ###
  cairo_pdf("./Figures/Structure_randomForest/RF_Phase_varImpPlot.pdf")
  varImpPlot(rf,
             main = "Element importance for phase structure\ncalculated with random forest")
  dev.off()
  
  tiff("./Figures/Structure_randomForest/RF_Phase_varImpPlot.tiff")
  varImpPlot(rf,
             main = "Element importance for phase structure\ncalculated with random forest")
  dev.off()
  
  ######################## 预测数据
  pred <- predict(rf, test_data)
  a <- data.frame(ID = test_data$ ID, 
                  Alloy_System = test_data$Alloy_Sys,
                  Alloy_Name = test_data$Alloy,
                  observed = test_data$Structure, 
                  predicted = pred)
  a <- arrange(a, observed)
  a <- a %>%
    mutate(trueValue = if_else(observed == predicted, 1,  0))
  
  evalStr <- function(a) {
    strLevel <- c("Amorphous", "B2 + σ", "BCC + σ", "BCC1 + B2", "Compounds", "FCC",
                  "FCC + BCC", "FCC + BCC +σ", "HCP")
    p <-  ggplot(a,aes(x = observed, y = predicted)) + 
      scale_y_discrete(limits = strLevel) +
      scale_x_discrete(limits = strLevel)  
    p1 <- p + geom_jitter(aes(x = observed, y = predicted, color = Alloy_System),
                          alpha = 0.6,
                          width = 0.1,
                          height = 0.1,
                          size = 20) 
    p1
    jitter_data <- layer_data(p1, 1)
    library(ggrepel)
    p1 <- p1 +    
      labs(x = NULL,
           y = NULL) +
      theme_bw() +
      theme(legend.position = "none",
            panel.background = element_blank(),
            legend.background =  element_rect(fill = "transparent",colour = NA),
            panel.grid = element_blank(),
            axis.text = element_text(size = 12),
            axis.line = element_line(size = 1)) +
      geom_abline(aes(group =1), 
                  slope = 1, intercept = 0,
                  color = "blue", size = 0.7) 
    p1
    ggsave("./Figures/Structure_randomForest/evalStr_RF0.pdf",
           width = 11.69,
           height = 8.27)
    p1 <- p1 + geom_text_repel(aes(x = jitter_data$x,
                                   y = jitter_data$y,
                                   label = Alloy_System,
                                   color = Alloy_System),
                               size = 5)
    p1
    ggsave("./Figures/Structure_randomForest/evalStr_RF1.pdf",
           width = 11.69,
           height = 8.27)
    
    p1 + coord_cartesian(xlim = c(5.98, 7.02),
                         ylim = c(5.98, 7.02))
    ggsave("./Figures/Structure_randomForest/evalStr_RF1672.pdf",
           width = 11.69,
           height = 8.27)
    p1 + coord_cartesian(xlim = c(7, 7),
                         ylim = c(7, 7)) 
    
    ggsave("./Figures/Structure_randomForest/evalStr_RF1673.pdf",
           width = 11.69,
           height = 8.27)
    p1 + coord_cartesian(xlim = c(6, 6),
                         ylim = c(6, 6)) 
    
    ggsave("./Figures/Structure_randomForest/evalStr_RF1676.pdf",
           width = 11.69,
           height = 8.27)
    ############################################
    strLevel <- c("Amorphous","Compounds", "BCC", "BCC1 + B2", "FCC",
                  "B2 + σ", "BCC + σ",  "FCC + BCC +σ",  
                  "FCC + BCC",  "HCP")
    p <-  ggplot(a,aes(x = observed, y = predicted)) + 
      scale_y_discrete(limits = strLevel) +
      scale_x_discrete(limits = strLevel) + 
      geom_jitter(aes(x = observed, y = predicted, color = Alloy_System),
                  alpha = 0.6,
                  width = 0.05,
                  height = 0.05,
                  size = 10) 
    p
    
    jitter_data <- layer_data(p, 1)
    library(ggrepel)
    p + geom_label_repel(aes(x = jitter_data$x,
                             y = jitter_data$y,
                             label = Alloy_System,
                             color = Alloy_System),
                         box.padding = 1,
                         point.padding = 0.2) +
      labs(x = "Actual structure",
           y = "Predicted structure") +
      geom_abline(aes(group =1),
                  slope = 1, intercept = 0,
                  color = "blue", size = 0.7)  +
      theme_bw() +
      theme(legend.position = "none",
            # panel.background = element_blank(),
            legend.background =  element_rect(fill = "transparent",colour = NA),
            # panel.grid = element_blank(),
            axis.text = element_text(size = 12),
            axis.title = element_text(size = 16),
            axis.line = element_line(size = 1)) 
    ggsave("./Figures/Structure_randomForest/real_pred_vis.pdf",
           device = cairo_pdf,
           width = 13,
           height = 8.034)
    ggsave("./Figures/Structure_randomForest/real_pred_vis.tiff",
           # device = cairo_pdf,
           width = 13,
           height = 8.034,
           dpi = 100)
  }
  evalStr(a)
  evalStrNeat <- function(a) {
    strLevel <- c("Amorphous","Compounds", "BCC", "BCC1 + B2", "FCC",
                  "B2 + σ", "BCC + σ",  "FCC + BCC +σ",  
                  "FCC + BCC",  "HCP")
    p <-  ggplot(a,
                 aes(x = observed, 
                     y = predicted)
    ) + 
      scale_y_discrete(limits = strLevel) +
      scale_x_discrete(limits = strLevel) + 
      geom_jitter(aes(x = observed, 
                      y = predicted,
                      color = factor(trueValue)
      ),
      width = 0.05,
      height = 0.05,
      size = 10) +
      scale_color_manual(values = c("red", "blue")) +
      geom_abline(aes(group =1),
                  slope = 1, 
                  intercept = 0,
                  color = "blue", 
                  size = 2,
                  alpha = 0.8)  
    p
    
    jitter_data <- layer_data(p, 1)
    library(ggrepel)
    p + geom_label_repel(aes(x = jitter_data$x,
                             y = jitter_data$y,
                             label = Alloy_System,
                             fill = factor(trueValue)
    ),
    color = "white",
    label.padding = 0.25,
    box.padding = 0.6,
    point.padding = 0.3,
    segment.color = "purple"
    ) +
      scale_fill_manual(values = c("red", "blue")) +
      labs(x = "Actual structure",
           y = "Predicted structure") +
      theme_bw(base_size = 16) +
      theme(panel.border = element_rect(size = 1.5),
            legend.position = "none",
            legend.background =  element_blank(),
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12,
                                     color = "black"),
            axis.line = element_line(size = 1)) 
    ggsave("./Figures/Structure_randomForest/real_pred_visNeat.pdf",
           device = cairo_pdf,
           width = 13,
           height = 8.034)
    ggsave("./Figures/Structure_randomForest/real_pred_visNeat.tiff",
           width = 13,
           height = 8.034)
  }
  evalStrNeat(a)
  
  library(xlsx)
  write.xlsx(a,
             "./Figures/Structure_randomForest/RF_Phase_Real_pred.xlsx")
  # print((sum(diag(a)))/sum(a))
  # 绘制测试数据的预测边距图，
  # 数据点的边距为正确归类后的比例减去被归到其他类别的最大比例。
  # 一般来说，边距为正数说明该数据点划分正确。
  cairo_pdf("./Figures/Structure_randomForest/RF_Phase_marginPlot.pdf")
  plot(randomForest::margin(rf, test_data$Structure),
       main = "Probability plot of the correctly predicted phase structure") +
    abline(h = 0,
           lty = 3)
  dev.off()
  
  
  ###ggplot2 varImportance map
  var_importance <- data_frame(variable= rownames(importance(rf)),
                               importance = as.vector(importance(rf, type = 2)))
  var_importance <- arrange(var_importance, importance)
  var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
  write.csv(var_importance,
            file = "./Data/varIm_Stru.csv")
  ggplot(var_importance,
         aes(x = importance, y = variable)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    labs(x = "MeanDecreaseGini",
         y = NULL) +
    theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.line = element_line(size = 0.8),
      panel.border = element_rect(size = 1),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 16),
      legend.title = element_text(size = 14),
      legend.text = element_text(size = 10)) 
  ggsave("./Figures/Structure_randomForest/varImportance_pahse_MeanDecreaseGini.pdf",
         device = cairo_pdf,
         width = 11.69,
         height = 8.27)
  periodic <- read.table("./Data/periodic-table.csv",
                         header = TRUE,
                         sep = ",")
  ####################################################################################
  varImHard <- read.csv("./Data/varIm_hard.csv")
  varImHard$variable <- factor(varImHard$variable, periodic$atomic_symbol)
  varImHard <- varImHard %>%
    select(2:3) %>%
    mutate(feature = "%IncMSE") 
  var_importance <- data_frame(variable= rownames(importance(rf)),
                               importance = as.vector(importance(rf, type = 2)))
  var_importance <- arrange(var_importance, importance)
  var_importance <- var_importance %>%
    mutate(feature = "MeanDecreaseGini") %>%
    rbind(varImHard)
  var_importance$variable <- factor(var_importance$variable, 
                                    levels = periodic$atomic_symbol)
  ########################
  p <- ggplot(var_importance,
              aes(x = variable, 
                  y = importance,
                  color = feature,
                  shape = feature,
                  linetype = feature,
                  group = feature)) +
    
    geom_point(size = 5,
               alpha = 0.7) +
    geom_line(size = 1) +
    scale_color_manual("",
                       values = c("blue", "red")) +
    scale_linetype_manual("",
                          values = c(1, 1)) +
    scale_shape_manual("",
                       values = c(15, 20)) +
    labs(x = NULL,
         y = NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(size = 0.8),
          panel.border = element_rect(size = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 18),
          legend.text.align = 0,
          legend.background =  element_rect(fill = "transparent",
                                            colour = NA, 
                                            size = 0.8),
          legend.position = c(0.85,0.85)) +
    geom_text(aes(x = 15.5,
                  y = 7),
              label = "Structure",
              color = "red",
              size = 6,
              check_overlap = TRUE) +
    geom_text(aes(x = "Mn",
                  y = 12),
              label = "Hardness",
              color = "blue",
              size = 6,
              check_overlap = TRUE)
  p
  # Get the ggplot grob
  g <- ggplotGrob(p)
  # Check out the grobs
  library(grid)
  grid.ls(grid.force(g))
  names.grobs <- grid.ls(grid.force(g))$name 
  labels <- names.grobs[which(grepl("^label", names.grobs))]
  fill <- c("blue", "red")
  for(i in seq_along(labels)) {
    g <- editGrob(grid.force(g), gPath(labels[i]), grep = TRUE,  
                  gp = gpar(col = fill[i]))
  }
  # Draw it
  tiff("./Figures/varImportance.tiff",
       width = 1000,
       height = 625)
  grid.newpage()
  grid.draw(g)
  dev.off()
  
  plotVarIm2y <- function(rf){
    
    varImHard <- read.csv("./Data/varIm_hard.csv")
    varImHard$variable <- factor(varImHard$variable, periodic$atomic_symbol)
    varImHard <- varImHard %>%
      select(2:3) %>%
      mutate(feature = "%IncMSE") 
    var_importance <- data_frame(variable= rownames(importance(rf)),
                                 importance = as.vector(importance(rf, type = 2)))
    var_importance <- arrange(var_importance, importance)
    var_importance <- var_importance %>%
      mutate(feature = "MeanDecreaseGini") %>%
      rbind(varImHard)
    var_importance$variable <- factor(var_importance$variable, 
                                      levels = periodic$atomic_symbol)
    
    
    tiff("./Figures/varImportance_2y.tiff",
         compression = "none",
         width = 3000,
         height = 2100,
         res = 300)
    ggplot(var_importance,
           aes(x = variable, 
               y = importance,
               color = feature,
               shape = feature,
               linetype = feature,
               group = feature)) +
      
      geom_point(size = 5, alpha = 0.9) +
      geom_line(size = 1) +
      scale_color_manual("",
                         values = c("blue", "red")) +
      scale_linetype_manual("",
                            values = c(1, 1)) +
      scale_shape_manual("",
                         values = c(15, 20)) +
      scale_y_continuous(sec.axis = sec_axis(~. *1, name = "Mean Decrease Gini")) +
      labs(x= NULL,
           y = "%IncMSE") +
      theme_bw() +
      theme(panel.grid = element_blank(),
            panel.border = element_rect(size = 1.5),
            
            axis.title = element_text(size = 16),
            axis.text = element_text(size = 12,
                                     color = "black"),
            axis.line = element_line(size = 0.8),
            axis.text.y = element_text(color = "blue"),
            axis.line.y = element_line(color = "blue"),
            axis.title.y = element_text(color = "blue"),
            axis.text.y.right = element_text(size = 12,
                                             color = "red"),
            axis.title.y.right = element_text(size = 16,
                                              color = "red"),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 18),
            legend.text.align = 0,
            legend.background =  element_rect(fill = "transparent",
                                              colour = NA, 
                                              size = 0.8),
            legend.position = "none") +
      geom_text(aes(x = "O",
                    y = 18),
                label = 'Element importance\n for hardness',
                color = "blue",
                size = 5,
                check_overlap = TRUE) +
      geom_text(aes(x = "Lu",
                    y = 18),
                label = 'Element importance\n for phase structure',
                color = "red",
                size = 5,
                check_overlap = TRUE) +
      geom_text(aes(x = "Al",
                    y = 15.23),
                label = "Al",
                color = "black",
                vjust = -1) +
      geom_text(aes(x = "Ti",
                    y = 19),
                label = "Ti",
                color = "black",
                vjust = -1) +
      geom_text(aes(x = "Cu",
                    y = 18.21),
                label = "Cu",
                color = "black",
                vjust = -1) +
      geom_text(aes(x = "Mo",
                    y = 15.98),
                label = "Mo",
                color = "black",
                vjust = -1) +
      geom_text(aes(x = "Ta",
                    y = 7.23),
                label = "Ta",
                color = "black",
                vjust = -1)
    dev.off()
    
  }
  plotVarIm2y(rf)
}
PredictStructure(BigMatrix)



