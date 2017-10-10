#### seperate Element and Weight
rename2system3 <- function(alloy) {
  
  alloy_old <- alloy
  Name <- c(alloy_old)
  
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
    Name[i] <- alloy_old
  }
  
  alloy
}