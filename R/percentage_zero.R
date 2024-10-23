percentage_zero <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")

    sapply(data, function(x) paste0(round(sum(x == 0, na.rm = TRUE) / nrow(data) *100, 2), " %"))
  }
}
