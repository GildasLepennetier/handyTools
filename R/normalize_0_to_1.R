#' Normalize values from any range to a range between 0 and 1
#' 
#' @name normalize_0_to_1
#'
#' @param x vector of numerical values, or column name from a piped data
#' @param na.rm option to consider the NA or not, default TRUE
#' @param all_same_return control over value to return when all same
#' BBmisc::normalize return 0.5, but in some cases we want to return 1
#' 
#' @return vector
#' 
#' @export
#' 
#' @examples
#' normalize_0_to_1(c(0, 1.87, 98, 1, 5, -50, -25))
#' \dontrun{
#' data.frame(x = c(0, 1.87, 98, 1, 5, -50, -25)) %>% 
#' mutate(y = normalize_0_to_1(x))
#' }
normalize_0_to_1 <- function(x, na.rm = TRUE, all_same_return = 0.5){
  if (length(unique(x)) == 1) {return(rep(all_same_return, length(x)))}
  (x - min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm)) 
  }
