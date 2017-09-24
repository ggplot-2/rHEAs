removeNum <- function(alloy){
  alloy <- as.character(alloy)
  library(stringr)
  alloy <- str_replace_all(alloy, "([A-Z])", "\\-\\1")
  alloy <- str_replace_all(alloy, "[0-9|.|x|(|)]", "-")
  alloy <- str_replace_all(alloy, "-+", " ")
  alloy
}
