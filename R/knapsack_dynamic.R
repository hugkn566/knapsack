#' Dynamic programming search for knapsack problem
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
#' results <- knapsack_dynamic(x = knapsack_objects[1:8,], W=3500)
#' @export


knapsack_dynamic <- function(x,W){
  stopifnot(is.data.frame(x)==TRUE)
  stopifnot(names(x)==c("w","v"))
  stopifnot(x[,1]>0 & x[,2] > 0)
  stopifnot(W > 0)
  n <- nrow(x)
  m <- matrix(0, nrow=n+1, ncol=W)
  for (i in 2:(n+1)){
    for (j in 1:W){
      if (x$w[i-1] > j){
        m[i, j] <-  m[i-1, j]
      }
      else{
        m[i, j] <- max(m[i-1, j], m[i-1, j-x$w[i-1]] + x$v[i-1])
      }
    }
  }

col <- W
row <- nrow(m)
items <- c()
while(row > 1){
if(m[row,col] != m[(row-1), col]){
  col <- col-x$w[row-1]
  items <- c(items, (row-1))
  }
row <- row-1
}

return(list(value=round(max(m)), elements=rev(items)))
}
