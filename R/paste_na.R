#' paste_na
#'
#' A function that paste data, similar to paste or paste0,
#' but avoid to have NA as string when the data is actually missing
#'
#' @param x a vector
#' @param sep default ','
#' @export
#' @return string

paste_na <- function(x, sep = ","){
  x <- as.character(x)
  x <- x[!is.na(x)]
  return(paste(x, collapse = sep))
}
