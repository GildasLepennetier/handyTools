#' safe-scrape
#'
#' A safe wrapper for scraping functions
#'
#' @param cmd an expression to be evaluated. Use curvy brackets
#' @param if_error value to return if error occurs
#'
#' @return depends on the cmd, generally a string
#' @export
safe_scrape <- function(cmd, if_error = NA_character_) {
	result <- tryCatch(
		cmd,
		error = function(e) {
			return(if_error)
		}
	)
	if (length(result) == 0) {
		return(if_error)
	} else {
		return(result)
	}
}
