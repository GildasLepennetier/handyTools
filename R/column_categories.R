#' Create categories for the column content.
#'
#' The quantiles define the boundaries to make groups
#'
#' The value "0" for the score 0 is taken automatically
#'
#' Using default quantiles: c(0.25, .75) and labels c("1", "2", "3") will
#' make a category = "1" for all values that are between 0 and first quartile,
#' category = "2" for all values between 0.25 and 0.75 and 3 for values > 0.75
#'
#' NA_as_zero = TRUE by default, so all zeros and NA will be in category "0"
#'
#'
#' @importFrom rlang enquo
#' @importFrom testthat test_that
#' @importFrom testthat expect_equal
#' @importFrom testthat expect_gt
#' @importFrom stats quantile
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom dplyr mutate
#' @importFrom dplyr if_else
#' @importFrom dplyr %>%
#' @importFrom dplyr distinct
#' @importFrom rlang :=
#'
#' @param df input data
#' @param col_in input column, typically "score" as final score
#' @param col_out output column, typically "cat_XXX" for the categories
#' @param quantiles limits in % of the data to break the col_in into
#' categories, default c(0.25, .5, .75)
#' @param labels string vector for the values between the quantiles,
#' default c("1", "2", "3")
#' @param NA_as_zero bool, default TRUE, to convert the NA to "0"
#' @param favour_lowest_score bool, default FALSE, if TRUE the intervals are closed on the left. This is important when the quantiles are very bad, and the cut function does not work well
#' @export
#' @examples
#' \dontrun{
#' tibble(medic_id = c(0, 0, 1, 2, 3), score_pub = c(0, NA, 0.1, 0.5, 1)) %>%
#' column_categories(score_pub, cat_publi, quantiles = c(0.25, .75),
#' labels = c("1", "2", "3"), NA_as_zero = T) %>%
#' column_categories(score_pub, cat_publi_2, quantiles = c(0.25, .75),
#' labels = c("1", "2", "3"), NA_as_zero = F)
#' }


column_categories <- function(df, col_in, col_out, quantiles = c(0.25, 0.75),
                              labels = c("1", "2", "3"), NA_as_zero = TRUE, favour_lowest_score = FALSE) {
  col_in <- enquo(col_in)
  col_out <- enquo(col_out)
  if ("0" %in% labels) {stop("The label zero is reserved for the score = 0 and eventually the NA")}
  if (0 %in% quantiles) {stop("The quantile = 0 should not be used (added auto)")}
  test_that("Values < 0 are unexpected when scoring, NA allowed", expect_equal(0, min(df %>% filter({{col_in}} < 0 & !is.na({{col_in}})) %>% nrow())))
  test_that("Not enough data to calculate categories, need at least 3 distinct values!", expect_gt(df %>% distinct({{col_in}}) %>% nrow(), 2))
  QUANTILES <- quantile(df %>% filter(!is.na({{col_in}}), {{col_in}} > 0) %>% pull({{col_in}}), probs = quantiles, na.rm = TRUE)
  BREAKS <- c(-Inf, 0, unname(QUANTILES), Inf)
  LABELS <- c("0", labels)
  df <- df %>%
    mutate({{col_out}} := as.character(cut({{col_in}}, breaks = BREAKS, right = favour_lowest_score, labels = LABELS)))
  if (NA_as_zero) {
    df <- df %>% mutate({{col_out}} := if_else(is.na({{col_in}}), "0", {{col_out}}))
  }
  return(df)
}
