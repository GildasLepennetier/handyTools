#' Extract json from column simple version
#'
#' @param df dataframe with a column json_data and a column id
#' @param .col column containing the json, default "json_data"
#' @param .id column containing the id, default "id"
#' @param if_error dataframe to return in case of error, default tibble()
#' @param filter_json filter to apply on the json after extraction, for
#' example when there is a list and we want only the "primary == TRUE"
#' @return dataframe with json extracted
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom dplyr slice select pull mutate as_tibble filter rename
#' @importFrom rlang parse_expr := .data
#' @importFrom tibble tibble
#'
#' @export

extract_json_v3 <- function(df, .col = "json_data", .id = "id", if_error = tibble(), filter_json = ""){
	# we expect a column json_data and a column id
	df <- map_df(1:nrow(df), function(i) {
		cmd <- {
			# get the json field
			JSON <- df %>% slice(i) %>% select(!!.col) %>% pull()
			if (JSON == "[]" | JSON == "") {return(if_error)}
			# convert json from string to df
			JSON <- jsonlite::fromJSON(JSON, flatten = T)
			if (filter_json != "") {
				JSON <- JSON %>% filter(!!rlang::parse_expr(filter_json))
			}
			# if there is an "id" column inside the json, this create a conflict
			if ("id" %in% names(JSON)) {
				JSON <- JSON %>% rename(json_id = .data$id)
			}
			# add the reference ID into the JSON
			ID <- df %>% slice(i) %>% pull(!!.id)
			JSON <- JSON %>% mutate(.data$id := ID) %>% as_tibble()
			return(JSON)
		}
		result <- tryCatch(cmd, error = function(e) {return(if_error)})
		if (length(result) == 0) {return(if_error)}
		else {return(result)}
	})
	return(df)
}


