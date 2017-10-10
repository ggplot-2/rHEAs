tidy_alloy <- function(Alloy_Vector) {
  
  library(dplyr)
  temp <- Alloy_Vector %>%
    data_frame(Alloy_Vector)
  names(temp) <- c("ID", "Alloy")
  View(temp)
  
  # library(tidytext)
  # td_Alloy <-   temp %>%
  #   unnest_tokens(word, temp)
}
