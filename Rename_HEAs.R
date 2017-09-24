################################
# Copyright: CC v3.2
# By Dinghao Miao, master of USTB
# This function is used to 
# rename the HEAs in a order way.
# No source() nor library()
################################
RenameHEAs <- function(alloy){
  
  periodic <- read.csv("periodic-table.csv", header = TRUE, sep = ",")
  alloy    <- factor(alloy, ordered = TRUE, levels = periodic$atomic_symbol)
  alloy    <- alloy[order(alloy)]
  newname  <- alloy[1]
  N        <- length(alloy) - 1
  for(i in 1:N) {
    newname <- paste(newname, alloy[i+1], sep = "-")
  }
  return(newname)
  
}
