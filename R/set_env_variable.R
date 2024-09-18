#' Set an environment variable
#' Set an environment variable. If the variable already exists,
#' it will not be overwritten unless the overwrite argument is set to TRUE.
#' @param key The name of the environment variable to set
#' @param value The value of the environment variable to set
#' @param overwrite If TRUE, environment variable will be overwritten
#' @return boolean
#' @export
#' @examples
#' set_env_variable("API_KEY", "your_api_key_here")
#' Sys.getenv("API_KEY")
#' Sys.unsetenv("API_KEY")
set_env_variable <- function(key, value, overwrite = FALSE) {
  existing <- Sys.getenv(names = T)
  if (length(key) != 1 | length(value) != 1) {stop("key and value must be length 1")}
  if (!is.character(key) | !is.character(value)) {stop("key and value must be character")}
  if (key %in% names(existing) & !overwrite) {stop(sprintf("Environment variable %s already exists", key))}
  do.call("Sys.setenv", structure(list(value), names = key))
  return(TRUE)
}

