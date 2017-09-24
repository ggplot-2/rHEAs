Alloy_info <- function(Alloy_Vector){
  Alloy_info <- extract_alloy(Alloy_Vector[1])
  for( i in 2:length(Alloy_Vector)) {
    Alloy_info <- Alloy_info %>%
      rbind(extract_alloy(Alloy_Vector[i]))
  }
  Alloy_info <- Alloy_info %>%
    distinct()
  Alloy_info
}