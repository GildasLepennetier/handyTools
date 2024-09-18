#' Retrieve holidays
#' Use a public API to retrieve holidays for a given country and language
#'
#' @param countryIsoCode ISO code of the country
#' @param languageIsoCode ISO code of the language
#' @param from_date start date
#' @param to_date end date
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr select
#' @export
#' @return data frame
#' @examples
#' get_holidays()

get_holidays <- function(from_date = format(Sys.Date(), "%Y-01-01") ,
												 to_date = format(Sys.Date(), "%Y-12-31"),
												 countryIsoCode = "DE", languageIsoCode = "DE"){
  url <- paste0("https://openholidaysapi.org/PublicHolidays",
                "?countryIsoCode=",countryIsoCode,
                "&languageIsoCode=",languageIsoCode,
                "&validFrom=",from_date,
                "&validTo=",to_date)
  public_holidays <- fromJSON(url) %>%
  	select("name", "type", "startDate", "endDate")

  return(public_holidays)
}
