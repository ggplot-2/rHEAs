library(reshape2)
generate_BigMatrix <- function(Alloy_info, HEAs){
  
  md <- melt(Alloy_info, id = c("Alloy_Sys", "Alloy", "Element", "Weight"))
  element_matrix <- dcast(md, Alloy~Element)
  element_matrix[is.na(element_matrix)] <- 0
  
  BigMatrix <- inner_join(HEAs, element_matrix, by = "Alloy")
  BigMatrix$Alloy_Sys <- BigMatrix$Alloy_Sys %>%
    as.factor()
  BigMatrix$Alloy <- BigMatrix$Alloy %>%
    as.factor()
  BigMatrix$Structure[BigMatrix$Structure == ""] <- NA
  BigMatrix$Density <- BigMatrix$Density %>%
    as.character() %>%
    as.numeric()
  BigMatrix$T <- BigMatrix$T %>%
    as.character() %>%
    as.numeric() %>%
    round()
  BigMatrix$T[is.na(BigMatrix$T)] <- 25
  BigMatrix$E <- BigMatrix$E %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Yield_Strength <- BigMatrix$Yield_Strength %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Yield_Strain <- BigMatrix$Yield_Strain %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Peak_Stress	<- BigMatrix$Peak_Stress %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Peak_Strain <- BigMatrix$Peak_Strain %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Fracture_Strength <- BigMatrix$Fracture_Strength %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Fracture_Strain <- BigMatrix$Fracture_Strain %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Compression_Yield_Strength <- BigMatrix$Compression_Yield_Strength %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Compression_Strain <- BigMatrix$Compression_Strain %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Strain <- BigMatrix$Strain %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Vickers_hardness. <- BigMatrix$Vickers_hardness. %>%
    as.character() %>%
    as.numeric()
  BigMatrix$Wear_Resistance <- BigMatrix$Wear_Resistance %>%
    as.character() %>%
    as.numeric()
  BigMatrix <- BigMatrix %>%
    group_by(Alloy_Sys, Alloy)
  return(BigMatrix)
}


