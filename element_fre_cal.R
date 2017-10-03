#### 计算元素词频，输出词频云图
#### 输出元素使用频率分布图
element_fre_cal <- function(Alloy_info) {
  library(wordcloud)
  library(scales)
  library(ggplot2)
  temp <-   Alloy_info %>%
    count(Element, sort = TRUE)
  
  pdf("./Figures/Element/elementCloud.pdf",
      paper = "a4r")
  Alloy_info %>%
    count(Element, sort = TRUE) %>%
  with(wordcloud(Element, n, random.order = FALSE, scale = c(5, 0.5),
                                   colors = c('red','blue','green',  'black', 'purple'),
                                   rot.per = 0,
                                   ordered.colors = FALSE))
  dev.off()
  
  elementFrequency <- Alloy_info %>%
    count(Element, sort = TRUE) %>%
    mutate(other = n / sum(n)) %>%
    ungroup()
  elementFrequency %>%
    rename(Symbol = Element, Fre = other) %>%
    select(1, 3) %>%
  write.csv(file = "./Data/elementFre.csv")
  ggplot(elementFrequency, aes(x = Element, y = other)) +
    geom_bar(stat = "identity",fill="#0000FF", colour="black") +
    labs(x = NULL, 
         y = "Frequency") +
    geom_text(aes(label = Element),
              check_overlap = TRUE,
              vjust = -1.0) +
    theme_bw() +
    theme(panel.grid = element_blank(),
          axis.title = element_text(size = 15),
          axis.text = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.line = element_line(size = 0.8),
          panel.border = element_rect(size = 1))
  ggsave("element_use_frequency.pdf",
         path = "./Figures/Element/",
         width = 297,
         height = 210,
         units = "mm")
  ggsave("element_use_frequency.tiff")
}
