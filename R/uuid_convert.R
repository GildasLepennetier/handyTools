#' convertion tool for the uuid
#'
#' @param id UUID (strings) to convert
#' @param hyphens logical, whether to include hyphens in the output (default: TRUE)
#' @return A string representation of the UUID, with or without hyphens
#' @export

uuid_convert <- function(id, hyphens = TRUE) {
	# Convert a UUID to a string representation
	id_str <- as.character(id)
	# remove hyphens by default
	id_str <- gsub("-", "", id_str)
	if (hyphens) {
		# Add hyphens to the UUID string
		id_str <- paste0(substr(id_str, 1, 8), "-",
										 substr(id_str, 9, 12), "-",
										 substr(id_str, 13, 16), "-",
										 substr(id_str, 17, 20), "-",
										 substr(id_str, 21, 32))
	}
	return(id_str)
}
