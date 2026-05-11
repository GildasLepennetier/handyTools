#' Connect DB
#'
#' A simple function that return a connection handle, using a config file yaml
#'
#' @param config_path config file path, e.g. "~/dbconfig.yml" ; file.edit("~/dbconfig.yml")
#' @param credential_name credential name, e.g. "aws_devel"
#' @param command command to run after connect, typical: set utf8
#'
#' @importFrom DBI dbConnect dbExecute
#' @importFrom RMariaDB MariaDB
#' @importFrom config get
#'
#' @return a connection handle
#' @export
connect_db <- function(credential_name = NULL, config_path = "~/dbconfig.yml", command = "set names utf8"){
	config <- config::get(file = config_path, value = credential_name)
	con <- DBI::dbConnect(
		RMariaDB::MariaDB(),
		host     = config$host,
		user     = config$user,
		password = config$password,
		port     = config$port,
		dbname   = config$dbname,
		ssl.mode = "required"   # encrypts but skips CA verification
	)

	if (!is.na(command) | is.null(command) | command != ""){
		DBI::dbExecute(con,command)
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

