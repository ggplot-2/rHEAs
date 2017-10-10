#### This function is employed to get element names employed in HEAs
get_element_names <- function(element_matrix) {
  variable <- names(element_matrix[2])
  for(i in 3:length(element_matrix)) {
    variable <- str_c(variable, names(element_matrix[i]), sep = " + ")
  }
  print(variable)
}

