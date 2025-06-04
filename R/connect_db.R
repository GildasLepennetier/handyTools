#' Connect DB
#'
#' A simple function that return a connection handle, using a config file yaml
#'
#' @param config_path config file path, e.g. "~/dbconfig.yml" ; file.edit("~/dbconfig.yml")
#' @param credential_name credential name, e.g. "aws_devel"
#' @param set_utf8 logical, whether to set utf8
#'
#' @importFrom DBI dbConnect dbExecute
#' @importFrom RMySQL MySQL
#' @importFrom config get
#'
#' @return a connection handle
#' @export
connect_db <- function(config_path, credential_name, set_utf8 = TRUE){
	config <- config::get(file = config_path, credential_name)
	con <- DBI::dbConnect(
		RMySQL::MySQL(),
		host = config$host,
		user = config$user,
		password = config$password,
		port   = config$port,
		dbname = config$dbname
	)
	if (set_utf8) {
		DBI::dbExecute(con,"set names utf8")
	}
	return(con)
}



# library(dbx)
# connect_db_dbx <- function(config_path, credential_name){
# 	config <- config::get(file = config_path, credential_name)
# 	con <- dbx::dbxConnect(
# 		adapter = "mysql",
# 		host = config$host,
# 		user = config$user,
# 		password = config$password,
# 		port   = config$port,
# 		dbname = config$dbname
# 	)
# 	dbx::dbxExecute(con,"set names utf8")
# 	return(con)
# }

