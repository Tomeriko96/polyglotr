#' Get the QCRI API key from the environment variable
#'
#' @return The QCRI API key stored in the QCRI_API_KEY environment variable.
qcri_api_key <- function() {
  api_key <- Sys.getenv("QCRI_API_KEY")
  if (api_key == "") {
    stop("QCRI_API_KEY environment variable is not set.")
  }
  return(api_key)
}
