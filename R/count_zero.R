count_zero <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")

    sapply(data, function(x) sum(x == 0, na.rm = TRUE))
  }
}
