#' Extract json from column
#' @param .df dataframe
#' @param .col column containing the json, default "properties"
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom dplyr bind_cols
#' @export
extract_json_v5 <- function(.df, .col){
	DF_result <- map_df(.df %>% pull({{.col}}), function(json_txt){
		tryCatch({
			DF1 <- fromJSON(json_txt, flatten = T)
			DF2 <- tibble(!!!DF1) # this transform the list of names values into a tibble
			return(DF2)
		}, error = function(e){
			return(tibble())
		})
	})
	return(bind_cols(.df, DF_result))
}
