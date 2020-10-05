#' Greedy heuristic search for knapsack problem
#' 
#' @param x An object of data.frame with 2 columns with colnames w and v.
#' @param W A single positive value.
#' @return The function returns a list containing the maximum knapsack value and which elements.
#' @examples
#' set.seed(42)
#'  n <- 2000
#'knapsack_objects <-
#'  data.frame(
#'    w=sample(1:4000, size = n, replace = TRUE),
#'    v=runif(n = n, 0, 10000)
#'  ) 
#' results <- greedy_knapsack(x = knapsack_objects[1:8,], W=3500)
#' @export

greedy_knapsack <- function(x,W){
  stopifnot(is.data.frame(x)==TRUE)
  stopifnot(names(x)==c("w","v"))
  stopifnot(x[,1]>0 & x[,2] > 0)
  stopifnot(W > 0)
  x$ratio <- x$v / x$w
  x <- x[order(x$ratio, decreasing = TRUE),]
  
  i <- 1
  items <- c()
  weight <- 0
  weight_temp <- 0
  value <- 0
  
  repeat{
    weight_temp <- weight + x$w[i]
    
    if(weight_temp < W){
      weight <- weight_temp
      value <- value + x$v[i]
      items <- c(items, as.numeric(rownames(x)[i]))
      i <- i+1
    } else {break}
  }
  return(list(value=round(value), elements=items))
}


