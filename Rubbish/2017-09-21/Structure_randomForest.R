Structure_randomForest <- function(BigMatrix) {
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
  ###### Random forest
  # set.seed(42)
  # cairo_pdf("./Figures/Structure_randomForest/RF_Phase_OOBError.pdf")
  # bestmtry <- tuneRF(norm_data[23:58],
  #                    norm_data$Structure,
  #                    stepFactor=1.5,
  #                    improve=1e-4,
  #                    ntree = 1000,
  #                    plot = TRUE,
  #                    doBest = TRUE,
  #                    oob.prox = TRUE)
  # dev.off()
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
  # write(rf, "./Figures/Structure_randomForest/RF_Phase_Model.txt")
  # ### MDSplot()函数用于实现随机森林的可视化
  # cairo_pdf("./Figures/Structure_randomForest/RF_Phase_treeVis.pdf")
  # MDSplot(rf, 
  #         fac = train_data$Structure, 
  #         k = 2)
  # dev.off()
  ### 计算模型变量重要性 ###
  cairo_pdf("./Figures/Structure_randomForest/RF_Phase_varImpPlot.pdf")
  varImpPlot(rf,
             main = "Element importance for phase structure\ncalculated with random forest")
  dev.off()
  
  #######################
  # 预测数据
  pred <- predict(rf, test_data)
  a <- table(observed = test_data$Structure, 
             predicated = pred)
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

}
  

