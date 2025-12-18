#' Split a dataframe into parts of size X
#'
#' @param .df dataframe to export
#' @param path_prefix path to the output file default: ./file suffix _partX.xlsx
#' @param slice_size slices size for the files
#' @importFrom dplyr slice
#' @importFrom writexl write_xlsx
#' @importFrom glue glue
#' @export
#'
export_sliced_excel <- function(.df, path_prefix = "./file", slice_size = 10000){
	slices_count <- ceiling(nrow(.df)/slice_size)
	for (i in seq_len(slices_count)){
		SUBSET <- .df %>% slice(((i-1)*slice_size+1):(i*slice_size))
		outfile <- glue("{path_prefix}_part{i}.xlsx")
		write_xlsx(SUBSET, outfile)
	}
}
