#' Extract json from column
#'
#' @param df dataframe
#' @param col column name to extract. Should not be a string, but a dplyr-column name
#' @param json_type either "array" or "object". In case of "name_repair" error,
#' first check that you do not have already a column with the same name as the
#' one you will create (ex: "name already exists" for any entity name)
#' Duplicated names will be repaired when the optin name_repair is used
#' @param names_repair option passed to unnest, check ?tidyr::unnest.
#' Advice: use "unique" or "universal" to repair, default to "check_unique"
#'
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom rlang enquo
#' @importFrom purrr map
#' @importFrom jsonlite fromJSON
#' @importFrom tidyr unnest
#' @export

extract_json_v2 <- function(df, col, json_type = "array", names_repair = "check_unique") {
  col = enquo(col)
  if (json_type == "array") {
    tmp_df <- df %>%
      filter(!is.na({{ col }})) %>%
      mutate(json = map({{ col }}, ~ fromJSON(.) %>% as.data.frame())) %>%
      unnest(.data$json, names_repair = names_repair) %>% #Column `.` doesn't exist: repair name differenty
      select(-{{ col }}) %>%
      rename({{ col }} := ".") %>% #TODO: bug in case of name repair, if name different than the one given in "col"
      mutate({{ col }} := trimws({{ col }}))
  } else if (json_type == "object") {
    tmp_df <- df %>%
      filter(!is.na({{ col }})) %>%
      mutate(json = map({{ col }}, ~ fromJSON(.) %>% as.data.frame())) %>%
      unnest(.data$json, names_repair = names_repair) %>%
      select(-{{ col }})
  } else {
    warning("json_type option: only array or object")
  }
  return(tmp_df)
}
