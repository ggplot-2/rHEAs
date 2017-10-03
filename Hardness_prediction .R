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
     # geom_text_repel(aes(x = actual,
     #                      y = pred_rf,
     #                      label = Alloy_System),
     #                  box.padding = 0.5,
     #                  color = "blue") +
     # geom_label_repel(aes(x = actual,
     #                     y = pred_BP,
     #                     label = Alloy_System),
     #                 box.padding = 0.5)
     #                 # color = "red"
     #                 # )
      
ggsave("./Figures/Hardness_prediction/Real_Predict.pdf",
         device = cairo_pdf,
         width = 11.69,
         height = 9)
  
  ggsave("./Figures/Hardness_prediction/Real_Predict.tiff",
         width = 13,
         height = 8.034,
         dpi = 100)
  
  ###ggplot2 varImportance map
  var_importance <- data_frame(variable= rownames(importance(rf)),
                              importance = as.vector(importance(rf, type = 1)))
  var_importance <- arrange(var_importance, importance)
  var_importance$variable <- factor(var_importance$variable, levels=var_importance$variable)
  write.csv(var_importance,
            file = "./Data/varIm_hard.csv")
  
  ggplot(var_importance,
         aes(x = importance, y = variable)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    labs(x = "%IncMSE",
         y = NULL) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.line = element_line(size = 0.8),
          panel.border = element_rect(size = 1),
          axis.text = element_text(size = 12),
          axis.title = element_text(size = 16),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 10))
  ggsave("./Figures/Hardness_prediction/varImportance_Structur_IncMSE.pdf",
         device = cairo_pdf,
         width = 11.69,
         height = 8.27)
  ggsave("./Figures/Hardness_prediction/varImportance_Structur_IncMSE.tiff",
         width = 243,
         height = 171,
         units = "mm",
         dpi = 100)
}


