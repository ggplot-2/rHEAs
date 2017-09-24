#### 将合金名称中间加上分隔符-####
rename2system <- function(alloy){
  
  library(stringr)
  library(dplyr)
  
  alloy <- alloy %>%
    str_replace_all("([A-Z])", "\\-\\1") %>%
    str_replace_all("-+", "-") %>%
    str_replace("-+", "")
  
  alloy
  
}
