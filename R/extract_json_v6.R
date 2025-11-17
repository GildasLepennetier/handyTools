#' Extract json from column
#'
#' @param .x dataframe
#' @param .id the ID column to use in case of line duplication
#' @param .col column with json to extract
#' @param relationship if some many-to-many are expected, use this option
#'
#' @importFrom dplyr pull
#' @importFrom dplyr as_tibble
#' @importFrom dplyr left_join
#' @importFrom rlang enquo
#' @importFrom purrr map2_dfr
#' @importFrom jsonlite fromJSON
#' @return dataframe with extracted json columns#'
#' @export
extract_json_v6 <- function (.x, .id, .col, relationship = NULL){
  .col = enquo(.col)
  .id = enquo(.id)
  df1 <- map2_dfr(.x %>% pull({{.id}}), .x %>% pull({{.col}}), .f = function(this_id, json) {
    this_json <- fromJSON(json, flatten = T)
    # if an id column exists, rename it
    if ("id" %in% colnames(this_json)) {this_json <- this_json %>% rename("json_id" = "id")}
    this_json$id = this_id # force the creation of an ID for merge, use the input .id
    return(this_json)
  })
  df2 <- .x %>% as_tibble() %>% left_join(df1, by = join_by({{.id}} == .data$id), relationship = relationship)
  return(df2)
}
