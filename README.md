
# handyTools

This R package contains a collection of useful functions.


## connect_db.R 

Create a connection to a database, based on a ~/dbconfig.yml
Set UTF-8 automatically

usage: 

	con <- connect_db("prod")

## expand_periods.R 

From a data frame with start and end date, this function create
a new data frame with a range of days between start and end.

## export_meeting_ICS.R

Create a list of ICS meetings, ready to import into a calender.

## export_xlsx.R

## extract_json.R

When a dataframe contains json fields, those are often annoying to unnext in R.
This provide a solution.

## get_holidays.R

## paste_na.R ; see also glue(.na = "")

## safe_scrape.R

Wrapper to handle missing data when extracting from a web page, e.g. using rvest 

## set_env_variable.R 

## show_in_excel.R 

Send a dataframe to this function, it will create a temporary excel file.

# installation

## using remote

	remotes::install_github("GildasLepennetier/handyTools")

## clone locally the repository

	git clone https://github.com/GildasLepennetier/handyTools.git
	devtools::install_local("~/Documents/Github/handyTools")

# check package

	devtools::check()
