#' Expand periods
#'
#' From a data frame with start and end date, this function create
#' a new data frame with a range of days between start and end.
#'
#' Steps:
#'   extract each start and end of periods
#'   create a range of days (see range_of)
#'   add the ID, rename output column, and join with the original data
#'
#' @param df data frame
#' @param id_col column name of the id
#' @param start_col column name of the start date (use as_datetime)
#' @param end_col column name of the end date
#' @param out_col column name of the output
#' @param range_of range to use to create the sequence. "day" by default
#'
#' @importFrom dplyr left_join
#' @importFrom purrr map_df
#' @importFrom tibble tibble
#' @importFrom rlang as_name
#' @importFrom dplyr rename_with
#' @importFrom dplyr mutate
#' @importFrom dplyr rename_with
#' @importFrom lubridate as_datetime
#'
#' @export
#' @return data frame
#'
expand_periods <- function(df, id_col = "id", start_col = "start",
													 end_col = "end", out_col = "day", range_of = "day") {
	# make sure ids are unique
	if (length(unique(df[[id_col]])) != nrow(df)) {stop("ids are not unique")}
	# convert start_col and end_col using as_datetime
	df[[start_col]] <- as_datetime(df[[start_col]])
	df[[end_col]] <- as_datetime(df[[end_col]])
	# create sequences
	map_df(1:nrow(df), function(.row_number){
		start <- df[[start_col]][.row_number]
		end <- df[[end_col]][.row_number]
		tmp <- tibble(tmp_name = seq(start, end, by = range_of)) %>%
			mutate(id = df[[id_col]][.row_number]) %>%
			rename_with(~ as_name(out_col), "tmp_name")
		return(left_join(tmp, df, by = as_name(id_col)))
	})
}
