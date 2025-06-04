#' Open a dataframe in an Excel file
#'
#' Pipe a dataframe to an xlsx file and save it maybe.
#'
#' @param data dataframe to show
#' @param show logical, whether to show the file at the end
#'
#' @importFrom writexl write_xlsx
#' @importFrom fs file_show
#' @export
#' @examples
#' \dontrun{
#' mtcars %>% filter(am == 1) %>% show_in_excel()
#' }
show_in_excel <- function(data, show = TRUE) {
	if (interactive()) { # avoid unwanted excel executions
		tmp <- paste0(tempfile(), ".xlsx")
		write_xlsx(data, tmp)
		if (show) {
			file_show(path = tmp)
		}
	}
	return(data)
}
