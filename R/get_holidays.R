#' Retrieve holidays
#' Use a public API to retrieve holidays for a given country and language
#'
#' @param countryIsoCode ISO code of the country
#' @param languageIsoCode ISO code of the language
#' @param from_date start date
#' @param to_date end date
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @importFrom tidyr unnest
#' @importFrom tibble as_tibble
#' @export
#' @return data frame
#' @examples
#' get_holidays()

get_holidays <- function(from_date = format(Sys.Date(), "%Y-01-01") ,
												 to_date = format(Sys.Date(), "%Y-12-31"),
												 countryIsoCode = "DE", languageIsoCode = "EN"){
  url <- paste0("https://openholidaysapi.org/PublicHolidays",
                "?countryIsoCode=",countryIsoCode,
                "&languageIsoCode=",languageIsoCode,
                "&validFrom=",from_date,
                "&validTo=",to_date)
  public_holidays <- jsonlite::fromJSON(url)
  public_holidays <- public_holidays %>%
  	tibble::as_tibble() %>%
  	tidyr::unnest(cols = c("name", "subdivisions"))

  return(public_holidays)
}
