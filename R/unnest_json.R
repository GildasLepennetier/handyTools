# Internal helper ---------------------------------------------------------

parse_json <- function(json, flatten = TRUE) {

	jsonlite::fromJSON(json, flatten = flatten) |>
		tibble::as_tibble()
}

# Internal helper ---------------------------------------------------------

prefix_names <- function(df, prefix) {
	names(df) <- paste(prefix, names(df), sep = "_")

	df
}

# Internal helper ---------------------------------------------------------

expand_json_column <- function(
		.x,
		.id,
		.col,
		flatten = TRUE
) {

	id_name  <- rlang::as_name(rlang::ensym(.id))
	col_name <- rlang::as_name(rlang::ensym(.col))

	purrr::map2_dfr(
		dplyr::pull(.x, {{ .id }}),
		dplyr::pull(.x, {{ .col }}),
		function(this_id, json) {

			res <- parse_json(json, flatten = flatten) |>
				prefix_names(
					prefix = col_name
				)

			res[[id_name]] <- this_id

			res
		}
	)
}

#' Unnest JSON from a column
#'
#' Extracts JSON content stored in a dataframe column and joins the
#' extracted fields back to the source dataframe.
#'
#' @param .x Dataframe.
#' @param .id Identifier column used to rejoin extracted data.
#' @param .col Column containing JSON strings.
#' @param relationship Relationship specification passed to
#'   [dplyr::left_join()].
#' @param rm_col Logical. Remove the source JSON column from the output.
#' @param flatten Logical. Passed to [jsonlite::fromJSON()].
#'
#' @return A dataframe with extracted JSON fields added as columns.
#'
#' @examples
#' df <- tibble::tibble(
#'   id = 1,
#'   properties = '{"name":"John","age":30}'
#' )
#'
#' unnest_json(
#'   df,
#'   id,
#'   properties
#' )
#'
#' @importFrom dplyr pull
#' @importFrom dplyr left_join
#' @importFrom dplyr select
#' @importFrom purrr map2_dfr
#' @importFrom rlang ensym
#' @importFrom rlang as_name
#'
#' @export
unnest_json <- function(
		.x,
		.id,
		.col,
		relationship = NULL,
		rm_col = TRUE,
		flatten = TRUE
) {

	id_name <- rlang::as_name(rlang::ensym(.id))

	expanded <- expand_json_column(
		.x = .x,
		.id = {{ .id }},
		.col = {{ .col }},
		flatten = flatten
	)

	result <- dplyr::left_join(
		.x,
		expanded,
		by = id_name,
		relationship = relationship
	)

	if (rm_col) {
		result <- dplyr::select(result, -{{ .col }})
	}

	result
}
