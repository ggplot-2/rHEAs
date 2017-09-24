BPFit_Hardness <- function(BigMatrix) {
  norm_data <- BigMatrix
  for(i in 8:19) {
    norm_data[, c(i)] <- scale(norm_data[, c(i)])
  }
  norm_data <- norm_data %>%
    rename(Hardness = Vickers_hardness.) 
  
  ##Feature plot #####
  library(caret)
  pdf("./Figures/NNet_Global/featurePlot_Hardness.pdf",
      height = 46.8,
      width = 33.1)
  featurePlot(x = norm_data[, 25:38],
              y = norm_data$Hardness)
  dev.off()
  
  formula <- names(norm_data[23:58]) %>%
    paste(collapse = "+")
  formula <- paste("Hardness ~", formula, sep = " ") %>%
    as.formula()
  
  # ##### BPNet ####
  library(neuralnet)
  set.seed(42)
  BPNNet <- neuralnet(formula,
                      data = norm_data %>%
                        filter(T == 25),
                      hidden = 7,
                      # rep = 15,
                      threshold = 0.03,
                      learningrate = 0.01,
                      algorithm = "rprop+",
                      err.fct = "sse",
                      act.fct = "tanh",
                      linear.output = FALSE)
 
  
  #### Plot ####
  # neuralnet
  pdf("./Figures/NNet_Global/Element_Hardness_nnet.pdf",
      paper = "a4r")
  plot(BPNNet,
       rep = "best")
  dev.off()
  # gwplot
  pdf("./Figures/NNet_Global/gwplot_Element_Hardness.pdf",
      height = 46.8,
      width = 33.1)
  layout(matrix(1:36, 
                nrow = 12, 
                byrow = T))
  for(i in 1:35) {
    gwplot(BPNNet,
           rep = NULL,
           selected.covariate = i,
           highlight = TRUE, 
           title = "Number of hidden nodes: 7") + 
      abline(h = 0,lty = 3) +
      abline(v = 0, lty = 3)
  }
  dev.off()
  ###### BPNNet_new #####
  set.seed(42)
  BPNNet_new <- neuralnet(Hardness ~ Al + Ti + V + Cr  + Fe + Co + Ni + Cu + Zr + Nb + Mo,
                          data = norm_data %>%
                            filter(T == 25),
                          hidden = 4,
                          # rep = 15,
                          threshold = 0.03,
                          learningrate = 0.01,
                          algorithm = "rprop+",
                          err.fct = "sse",
                          act.fct = "tanh",
                          linear.output = FALSE)

  
  ### Plot ####
  neuralnet
  pdf("./Figures/NNet_Global/Element_Hardness_nnet_new.pdf",
      paper = "a4r")
  plot(BPNNet_new,
       rep = "best")
  dev.off()
  # gwplot
  pdf("./Figures/NNet_Global/gwplot_Element_Hardness_new.pdf",
      height = 46.8,
      width = 33.1)
  layout(matrix(1:12, 
                nrow = 4, 
                byrow = 4))
  for(i in 1:11) {
    gwplot(BPNNet_new,
           selected.covariate = i,
           highlight = TRUE, 
           title = "Number of hidden nodes: 4") + 
      abline(h = 0,lty = 3) +
      abline(v = 0, lty = 3)
  }
  dev.off()
  
  ggplot() +
    geom_point(aes(x = as.vector(BPNNet$response),
                   y = unlist(BPNNet$net.result),
                   color = "Neuralnet\n(Full elements)")) +
    geom_smooth(aes(x = as.vector(BPNNet$response),
                    y = unlist(BPNNet$net.result),
                    color = "Neuralnet\n(Full elements)"),
                method = "loess") +
    geom_point(aes(x = as.vector(BPNNet_new$response),
                   y = unlist(BPNNet_new$net.result),
                   color = "Neuralnet\n(Part elements)")) +
    geom_smooth(aes(x = as.vector(BPNNet_new$response),
                    y = unlist(BPNNet_new$net.result),
                    color = "Neuralnet\n(Part elements)"),
                method = "loess") +
    labs(x = "Real values(Normalized)",
         y = "Predicted values(Normalized)",
         title = "Hardness") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_abline(intercept = 0, slope = 1) 
  ggsave("./Figures/NNet_Global/Hardness_Real_Prediction.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
  a <- table(as.vector(BPNNet$response),
             actua = unlist(BPNNet$net.result))
  print((sum(diag(a)))/sum(a))

  b <- table(as.vector(BPNNet_new$response),
            actua = unlist(BPNNet_new$net.result))
  print((sum(diag(b)))/sum(b))
  # #### BPNNet_new1 
  BPNNet_new1 <- neuralnet(Hardness ~ Al + Ti + V + Cr  + Fe + Co + Ni  + Mn + B ,
                           data = norm_data ,
                           hidden = 4,
                           # rep = 15,
                           threshold = 0.03,
                           learningrate = 0.01,
                           algorithm = "rprop+",
                           err.fct = "sse",
                           act.fct = "tanh",
                           linear.output = FALSE)


  ### Plot ####
  neuralnet
  pdf("./Figures/NNet_Global/Element_Hardness_nnet_new1.pdf",
      paper = "a4r")
  plot(BPNNet_new1,
       rep = "best")
  dev.off()
  # gwplot
  pdf("./Figures/NNet_Global/gwplot_Element_Hardness_new1.pdf",
      height = 46.8,
      width = 33.1)
  layout(matrix(1:9,
                nrow = 3,
                byrow = 3))
  for(i in 1:9) {
    gwplot(BPNNet_new1,
           selected.covariate = i,
           highlight = TRUE,
           title = "Number of hidden nodes: 4") +
      abline(h = 0,lty = 3) +
      abline(v = 0, lty = 3)
  }
  dev.off()
  c <- table(as.vector(BPNNet_new1$response),
             actua = unlist(BPNNet_new1$net.result))
  print((sum(diag(c)))/sum(c))
  write.csv(BPNNet$result.matrix, 
            "./Figures/NNet_Global/BPNNet$result.matrix.txt")
  write.csv(BPNNet_new$result.matrix, 
            "./Figures/NNet_Global/BPNNet_new$result.matrix.txt")
  write.csv(BPNNet_new1$result.matrix, 
            "./Figures/NNet_Global/BPNNet_new1$result.matrix.txt")
  
}