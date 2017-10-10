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
  
  tiff("./Figures/Structure_randomForest/RF_Phase_varImpPlot.tiff")
  varImpPlot(rf,
             main = "Element importance for phase structure\ncalculated with random forest")
  dev.off()
  
  #######################
  # 预测数据
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
  

