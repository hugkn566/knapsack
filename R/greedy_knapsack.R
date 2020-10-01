

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


