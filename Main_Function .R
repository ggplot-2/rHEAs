rm(list = ls())
#### Library load & Source ####
library(xlsx)
library(stringr)
library(dplyr)
library(ggplot2)
#### Import HEAs Dataset ####
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/rename2system.R')
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/rename2system2.R')
HEAs <- read.xlsx2("./Data/High-entropy alloy system.xlsx",
                   sheetName = "Main",
                   encoding = "UTF-8") %>%
                   select(-contains("X.")) %>%
                   mutate(Alloy = rename2system(Alloy))  %>%
                   mutate(Alloy_Sys = rename2system2(Alloy))  %>%
                   select(Alloy_Sys, everything()) %>%
                   distinct()
#### Generate alloy info of components and a BigMatrix of HEAs Data####
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/generate_Alloy_info.R')
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/generate_BigMatrix.R')
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/extract_alloy.R', encoding = 'UTF-8')
Alloy_info <- generate_Alloy_info(HEAs)
BigMatrix <- generate_BigMatrix(Alloy_info, HEAs)
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/cluster_alloy.R')
ID <- cluster_alloy(HEAs, Alloy_info)
BigMatrix <- BigMatrix %>%
  inner_join(ID, by = "Alloy_Sys") 
BigMatrix <- BigMatrix %>%
  select(ID, everything())
### Term Frequency analysis to find out the important element ####
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/element_fre_cal.R')
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/get_element_names.R')
element_fre_cal(Alloy_info)
#### graph ####
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/graphs_generate.R')
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/BPFit_Hardness.R')
graphs_generate(BigMatrix)
#### Hardness analysis  ####
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/Hardness_prediction .R', encoding = 'UTF-8')
Hardness_prediction(BigMatrix) 
#### Phase formation rules#####
source('~/GitHub/HEAsDeepDive/rHEAsDeepDive/Structure_randomForest.R', encoding = 'UTF-8')
Structure_randomForest(BigMatrix)

