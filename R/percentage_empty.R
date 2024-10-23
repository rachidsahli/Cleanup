percentage_empty <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")

    sapply(data, function(x) paste0(round(sum(x == "", na.rm = TRUE) / nrow(data) * 100, 2), " %"))
  }
}
