#' Optimized brute force search for knapsack problem
#' 
#' @param x An object of data.frame with 2 columns with colnames w and v.
#' @param W A single positive value.
#' @param parallel If TRUE parallelize the function across detected cores, default is FALSE.
#' @return The function returns a list containing the maximum knapsack value and which elements.
#' @examples
#' set.seed(42)
#'  n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  ) 
#' results <- brute_force_knapsack(x = knapsack_objects[1:8,], W=3500)
#' @export
brute_force_knapsack <- function(x,W, parallel = FALSE){
  stopifnot(is.data.frame(x)==TRUE)
  stopifnot(names(x)==c("w","v"))
  stopifnot(x[,1]>0 & x[,2] > 0)
  stopifnot(W > 0)

  search_func <- function(a){
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
          if(weights > W){break}
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
  
  
  if(parallel == TRUE) {
    n_cores <- parallel::detectCores()
    clust <- parallel::makeCluster(n_cores)
    parallel::clusterExport(clust, "bitmat", envir = environment())
    results <- parallel::parLapply(cl = clust, 1:2^nrow(x)-1, fun = search_func)
    parallel::stopCluster(clust)
    return(results[[1]])
    
  }else{
  
  
  
  return(search_func())
  }
}
