#' Brute force search for knapsack problem

brute_force_knapsack_unopt <- function(x,W){
  stopifnot(is.data.frame(x)==TRUE)
  stopifnot(names(x)==c("w","v"))
  stopifnot(x[,1]>0 & x[,2] > 0)
  stopifnot(W > 0)
  
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
