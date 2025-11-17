#' Extract json from a text string
#' @param json_txt text string containing the json
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @export
extract_json_from_txt <- function(json_txt){
	#cli_alert_info("extracting json from text: {json_txt}")
	DF <- jsonlite::fromJSON(json_txt, flatten = T)
	DF2 <- tibble(!!!DF) # this transform the list of names values into a tibble
	return(DF2)
}

#' Extract json from column
#' @param .df dataframe
#' @param .col column containing the json, default "properties"
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @export
extract_json_v4 <- function(.df, .col = "properties"){map_df(.df %>% pull({{.col}}), extract_json_from_txt)}
