rm(list = ls())
shaolei <- as.data.frame(read.table("./Data/shaolei.txt"))
library(ggplot2)
library(neuralnet)
set.seed(42)
BPNet_shaolei <- neuralnet(ComressiveStrength + Strain ~ Mg + Al + Cu + Zn + Si,
                   data = shaolei,
                   hidden = 10,
                   threshold=0.01,
                   rep = 15,
                   learningrate = 0.03,
                   algorithm = "backprop",
                   err.fct = "sse",
                   act.fct = "tanh",
                   linear.output = FALSE)
pdf("./Figures/邵磊/BP-Neural Net model for Mg-Al-Cu-Zn-Si.pdf",
    paper = "a4")
plot(BPNet_shaolei, 
     rep = "best")
dev.off()

pre <- BPNet_shaolei$net.result[[1]]
pre <- as.data.frame(BPNet_shaolei$net.result[[1]])
pre <- pre %>%
  mutate(ID = 1:dim(pre[1])) %>%
  select(ID, everything())
result <- merge.data.frame(shaolei, pre, by = "ID")
result <- result %>%
  mutate(ComressiveStrength = ComressiveStrength * 177.5812 + 554.8544,
         V1 = V1 * 177.5812 + 554.8544,
         Strain = Strain * 12.16864 + 12.75278,
         V2 = V2 * 12.16864 + 12.75278)

library(ggplot2)
ggplot(data = result, aes(ComressiveStrength, V1)) +
  geom_point() +
  geom_line(aes(V1, V1)) +
  labs(x = "Compressive Strenth/MPa\n(Experimental value)", 
       y = "Compressive Strenth/Mpa\n(Prediction value with BP-Neural Net)")
ggsave("./Figures/邵磊/shaolei_Strenth.pdf",
       width = 297, height = 210, units = "mm")

ggplot(data = result, aes(Strain, V2)) + 
  geom_point() +
  geom_line(aes(Strain, Strain)) +
  labs(x = "Strain/%\nExperimental value",
       y = "Strain/%\nPrediction value with BP-Neural Net")
ggsave("./Figures/邵磊/shaolei_Strain.pdf",
       width = 297, height = 210, units = "mm")


prediction(BPNet_shaolei)


pdf("./Figures/邵磊/gwplot_shaolei.pdf",
    width = 11.69,
    height = 16.53)
    # height = 46.8,
    # width = 33.1)
layout(matrix(1:10, 
              nrow = 5, 
              byrow = T))
for(i in 1:5) {
  gwplot(BPNet_shaolei,
         # rep = "best",
         selected.covariate = i,
         selected.response = 1,
         highlight = TRUE)
  gwplot(BPNet_shaolei,
         selected.covariate = i,
         selected.response = 2,
         highlight = TRUE)
}
dev.off()

