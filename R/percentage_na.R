


percentage_na <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")
  }
  sapply(data, function(x) paste0(round(sum(is.na(x)) / nrow(data) * 100, 2), " %"))
}
