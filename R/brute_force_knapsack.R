


knapsack_brute_force <- function(x,W){
  
  n <- nrow(x)
  results <- list(value=0, elements=c())
  
  bitmat <- matrix(0, nrow=n, ncol=2^nrow(x)-1)
  
  for (combs in 1:ncol(bitmat)) {
    bitmat[,combs] <- as.integer(intToBits(combs)[1:n])
  }
  
  for(col in 1:ncol(bitmat)){
    weights <- 0
    values <- 0
    items <- c()
    
    for (row in 1:n) {
      if(bitmat[row,col]==1){
        weights <- weights + x[row,1]
        values <- values + x[row,2]
        items <- c(items, row)
      }
    }
    if(weights <= W & values > results[[1]]){
      results[[1]] <- round(values)
      results[[2]] <- items
    }
  }
  
  return(results)
}
a <- proc.time()
knapsack_brute_force(x=knapsack_objects[1:16,],W)
b <- proc.time()
b-a