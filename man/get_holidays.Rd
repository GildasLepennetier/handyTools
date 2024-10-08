% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_holidays.R
\name{get_holidays}
\alias{get_holidays}
\title{Retrieve holidays
Use a public API to retrieve holidays for a given country and language}
\usage{
get_holidays(
  from_date = format(Sys.Date(), "\%Y-01-01"),
  to_date = format(Sys.Date(), "\%Y-12-31"),
  countryIsoCode = "DE",
  languageIsoCode = "EN"
)
}
\arguments{
\item{from_date}{start date}

\item{to_date}{end date}

\item{countryIsoCode}{ISO code of the country}

\item{languageIsoCode}{ISO code of the language}
}
\value{
data frame
}
\description{
Retrieve holidays
Use a public API to retrieve holidays for a given country and language
}
\examples{
get_holidays()
}
