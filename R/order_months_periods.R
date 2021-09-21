order_months_periods <- function(data_as_list){
  
  data_as_list <- lapply(data_as_list, function(x){
    colnames(x) <-  as.Date(as.yearmon(colnames(x), "%Y%b"))
    x <- x[,order(colnames(x))]
    colnames(x) <- sapply(as.character(as.yearmon(colnames(x), "%Y-%m-%d")), function(y) {
      y <- strsplit(y, ' ')[[1]]
      y <- paste0(y[2], y[1])
    })
    return(x)
  })
  
  return(data_as_list)
  
}