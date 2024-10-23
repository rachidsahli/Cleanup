
count_empty <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")

    sapply(data, function(x) sum(x == "", na.rm = TRUE))
  }
}
