library(latex2exp)
graphs_generate <- function(BigMatrix)
{
  ##### Phase Structure distribution ######
  ggplot(BigMatrix, aes(x = reorder(Structure, 
                                    table(Structure)[Structure]))) +
    geom_bar(data = subset(BigMatrix, !is.na(Structure)), aes(fill = factor(ID))) +
    coord_flip() +
    scale_fill_discrete(name = "Alloy ID") +
    labs(x = "Structure")
  ggsave("./Figures/Distribution/Structure_distribution.pdf",
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
  ggsave("./Figures/Distribution/Alloy_hardness.pdf",
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
  #### Hardness - Yield Strength #####
  # BigMatrix_model <- dlply(BigMatrix, .(interaction(T, ID)), 
  #                          lm(na.action = na.exclude), 
  #                          formula = Yield_Strength ~ Vickers_hardness.)
  # BigMatrix_coef <- ldply(mtcars_model, coef)
  # names(BigMatrix_coef)[2:3] <- c("intercept", "slope")
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
  
  ggsave("./Figures/Distribution/Hardness_YieldStrength.pdf",
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
  ggsave("./Figures/Distribution/Wear resistance_hardness.pdf",
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
  ggsave("./Figures/Distribution/Ranks_Vickers_hardness.pdf",
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
  ggsave("./Figures/Distribution/Ranks_Wear_Resistance.pdf",
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
  ggsave("./Figures/Distribution/Ranks_Young's modulus of elasticity.pdf",
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
  ggsave("./Figures/Distribution/Ranks_Yield_Strength.pdf",
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
  ggsave("./Figures/Distribution/Ranks_Peak_Strengt.pdf",
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
  
  ggsave("./Figures/Distribution/Ranks_Fracture_Strength.pdf",
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
  
  ggsave("./Figures/Distribution/Ranks_Compression_Yield_Strength.pdf",
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
  
  ggsave("./Figures/Distribution/Ranks_Fracture_Strain.pdf",
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
  
  ggsave("./Figures/Distribution/Ranks_Peak_Strain.pdf",
         device = cairo_pdf,
         height = 210,
         width = 297,
         units = "mm")
  
  
  
  
  
  
}








