#### 提取合金中的元素向量和权重向量####
#### library(stringr)
#### library(dplyr)
periodic <- read.table("./Data/periodic-table.csv",
                       header = TRUE,
                       sep = ",") 

extract_alloy <-  function(alloy){

  alloy_old <- alloy
  Alloy <- c(alloy_old)
  alloy <- alloy %>%
    str_replace_all("(\\s)+", "")
  alloy <- alloy %>%
    str_replace_all("(\\D-)", "\\11-")
  alloy <- alloy %>%
    str_replace_all("(\\s)+", "")
  alloy <- alloy %>%
    str_replace_all("(\\D)$", "\\1-1")
  alloy <- alloy %>%
    str_replace_all("x", "")
  alloy <-  as.vector(alloy[[1]])
  N <- length(alloy)
  for(i in 1:N) {
    alloy[i] <- str_replace_all(alloy[i], "(\\d+)", "-\\1")
  }
  alloy <- alloy %>%
    str_replace_all("\\.-", ".")
  alloy <- alloy %>%
    str_replace_all("-+", "-")
  N <- length(alloy)
  for(i in 1:N) {
    alloy[i] <- str_split(alloy[i], "-")
    Alloy[i] <- alloy_old
  }

   alloy <- as.data.frame(matrix(unlist(alloy), ncol = 2, byrow = T))
   
  alloy <- alloy %>%
    rename(Element = V1, Weight = V2)

  Element <- factor(as.vector(unlist(alloy[1])),
                    periodic$atomic_symbol)
  Weight <- as.numeric(as.vector(unlist(alloy[2])))

  df <- data.frame(Alloy, Element, Weight)

  df <- df %>%
    mutate(Weight_atom = Weight/sum(Weight)) %>%
    select(everything(), Weight_atom)
  df <- df %>%
    mutate(Alloy_Sys = rename2system2(Alloy)) %>%
    select(Alloy_Sys, everything())

  df
}
  
