generate_Alloy_info <- function(HEAs) {
  r <- dim(HEAs)[1]
  Alloy_info <- extract_alloy(HEAs[1, 2])
  for( i in 2:r) {
    Alloy_info <- Alloy_info %>%
      rbind(extract_alloy(HEAs[i, 2]))
  }
  Alloy_info <- Alloy_info %>%
    distinct()
  return(Alloy_info)
}