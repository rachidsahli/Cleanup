detect_outliers <- function(data){
  if (!is.data.frame(data)){
    stop("The argument must be a dataframe.")}

  outliers_list <- list()

  for (col in names(data)){
    x <- data[[col]]

    if (is.numeric(x)){

      x <- x[!is.na(x)]

      Q1 <- quantile(x, 0.25, na.rm = TRUE)
      Q3 <- quantile(x, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1

      lower_bound <- Q1 - 1.5 * IQR
      upper_bound <- Q3 + 1.5 * IQR

      outliers <- x[x < lower_bound | x > upper_bound]

      if (length(outliers) == 0) {
        outliers_list[[col]] <- "No outliers detected"
      } else {
        outliers_list[[col]] <- outliers
      }
    }
  }

  for (col in names(outliers_list)) {
    cat("\n$", col, ":\n", sep = "")
    if (is.character(outliers_list[[col]])) {
      cat("  - ", outliers_list[[col]], "\n")
    } else {
      cat("  - Outliers detected (Value) : ", paste(outliers_list[[col]], collapse = ", "), "\n")
    }
  }

  return(invisible(outliers_list))
}
