#Hardness_prediction
Hardness_prediction <- function(BigMatrix) {
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
  # cairo_pdf("./Figures/Hardness_prediction/RF_before tuning.pdf")
  cairo_ps("./Figures/Hardness_prediction/RF_before tuning.ps")
  plot(rf,
       main = "Random forest - before tuning")
  dev.off()
  
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
  importance(rf,type=1)
  # cairo_pdf("./Figures/Hardness_prediction/varImpPlot.pdf")
  jpeg("./Figures/Hardness_prediction/varImpPlot.jpg")
  varImpPlot(rf,
             main = "Element importance calculated with random forest")
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
  ggplot(data = data.frame(actual = test_hard_unscale,
                           pred_rf = pred_rf_unscale,
                           pred_BP = pred_BP_unscale)) +
    geom_point( aes(x = actual,
                    y = pred_rf,
                    color = "pred_rf")) +
    coord_equal() +
    geom_abline(color = "black") +
    
    labs(x = TeX("Actual vickers hardness/kg$\\cdot$mm^{-2}"),
         y = TeX("pred_rficted vickers hardness/kg$\\cdot$mm^{-2}")) +
    geom_point(aes(x = actual,
                   y = pred_BP, 
                  color = "pred_BP")) +
    scale_color_discrete(labels = c("Random forest fit\n R2 = 0.58\nRMSE = 119.55\n",
                                    "BP-neural network fit\n R2 = 0.65\nRMSE = 108.85"
                                     )) +
    guides(color=guide_legend(title=NULL))  
  ggsave("./Figures/Hardness_prediction/Real_Predict.pdf",
         device = cairo_pdf)
  ggsave("./Figures/Hardness_prediction/Real_Predict.jpg")
  ggsave("./Figures/Hardness_prediction/Real_Predict.tiff")
 
    
    
    

}


