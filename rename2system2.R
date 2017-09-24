#### 去除合金中的数字 ####
rename2system2 <- function(alloy){
  
  library(stringr)
  library(dplyr)
  
  alloy <- alloy %>%
    str_replace_all("([0-9|.|x|(|)]+)", "") 

  alloy
  
}
