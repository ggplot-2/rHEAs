count_result <- function(result,data_test){  
  n <- length(result)  
  count_right <- 0  
  i <- 1  
  for (i in 1:n){  
    if (result[i]==data_test[i,17]){  
      count_right = count_right+1  
    }  
  }  
  print(count_right/n)  
}  