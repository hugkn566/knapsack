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
