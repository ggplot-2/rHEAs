#### 计算元素词频，输出词频云图
#### 输出元素使用频率分布图
element_fre_cal <- function(Alloy_info) {
  library(wordcloud)
  library(scales)
  library(ggplot2)
  temp <-   Alloy_info %>%
    count(Element, sort = TRUE)
  
  pdf("./Figures/element_cloud.pdf", 
      paper = "a4r")
  Alloy_info %>%
    count(Element, sort = TRUE) %>%
  with(wordcloud(Element, n, random.order = FALSE, scale = c(5, 0.5),
                                   colors = c('red','blue','green',  'black', 'purple'),
                                   rot.per = 0,
                                   ordered.colors = FALSE))
  dev.off()

  frequency <- Alloy_info %>%
    count(Element, sort = TRUE) %>%
    mutate(other = n / sum(n)) %>%
    ungroup()
  # element_distribution <- ggplot(frequency, aes(x = Element, y = other)) +
  #   geom_point() +
  #   geom_hline(yintercept = 0.025) +
  #   geom_vline(xintercept = 4.5) +
  #   geom_vline(xintercept = 19.5) +
  #   geom_text(aes(label = Element), check_overlap = TRUE, vjust = -1.0) +
  #   theme(legend.position="none") +
  #   labs(y = "Frequency", x = NULL)
  element_distribution <- ggplot(frequency, aes(x = Element, 
                                                y = other,
                                                fill = Element)) +
    geom_bar(stat="identity") +
    labs(x = NULL,
         y = "Frequency") +
    geom_text(aes(label = Element), check_overlap = TRUE, vjust = -1.0) +
    theme(legend.position = "none")
  # print(element_distribution)
  # ggsave("element_distribution.pdf", element_distribution, path = "./Figures/",
  #        width = 297, height = 210, units = "mm")
  ggsave("element_distribution_no_legend.pdf", 
         element_distribution + theme(legend.position = "none"), 
         path = "./Figures/",
         width = 297, height = 210, units = "mm")
}
