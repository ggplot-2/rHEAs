scale_BigMatrix <- function(BigMatrix) {
  BigMatrix <- generate_BigMatrix(Alloy_info, HEAs)
  s <- c(0)
  mu <- c(0)
  for(j in 7:18) {
    BigMatrix$ <- BigMatrix[j] %>%
      scale()
    # s[j-6] <- attr(BigMatrix[j],"scaled:scale")
    # mu[j-6] <- attr(BigMatrix[j],"scaled:center")
    print(attr(BigMatrix[j],"scaled:scale"))
  }
  as.data.frame(s, mu)
}