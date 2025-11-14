#' Extract json from column
#'
#' @param .x dataframe tile tibble()
#' @param .id column id to use to extract data properly
#' @param .col column containing the json
#' @param in_parallel if TRUE, use furrr::future_map2_dfr instead of purrr::map2_dfr
#' @param workers number of workers to use, default 12
#' @param relationship if not NULL, you probalby need "many-to-many"
#'
#'
#' @importFrom rlang enquo
#' @importFrom dplyr pull left_join join_by
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map2_dfr
#' @importFrom furrr future_map2_dfr
#' @importFrom future plan
#' @importFrom tibble as_tibble
#' @importFrom utils data
#'
#' @export
extract_json_v1 <- function(.x, .id, .col, in_parallel = F, workers = 12, relationship = NULL) {
  .col = enquo(.col) # any json field form the data frame
  .id = enquo(.id) # the id of the entity as name
  if (in_parallel) {
    future::plan("multisession", workers = workers)
    df1 <- future_map2_dfr(.x %>% pull({{.id}}),.x %>% pull({{.col}}),
                                  .f = function(id, json){
                                    this_json <- jsonlite::fromJSON(json, flatten = T) #flatten remove some "null" values and avoid a crash
                                    this_json$id = id
                                    return(this_json)
                                  })
  }else{
    df1 <- map2_dfr(.x %>% pull({{.id}}),.x %>% pull({{.col}}),
                    .f = function(id, json){
                      this_json <- jsonlite::fromJSON(json, flatten = T) #flatten remove some "null" values and avoid a crash
                      this_json$id = id
                      return(this_json)
                    })
  }
  # ignore NULL things example properties
  #or replace by NA?
  df2 <- .x %>%
    as_tibble()	%>%
    left_join(df1 , by = join_by({{.id}} == data$id), relationship = relationship) # the find may need many-to-many
  return(df2)
}


