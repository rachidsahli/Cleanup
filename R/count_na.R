#' Count na
#'
#' @param data  a dataframe
#'
#' @return the number of missing values for each variable in the dataframe
#' @export
#'
#' @examples
#'
count_na <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")
  }
  sapply(data, function(x) sum(is.na(x)))
}
