#' Perform a GET request and return parsed JSON
#'
#' @param url Character. The request URL.
#' @param query Named list of query parameters.
#' @param headers Named character vector of additional HTTP headers.
#'
#' @return Parsed JSON response as a list.
#' @keywords internal
http_get_json <- function(url, query = list(), headers = character()) {
  req <- httr2::request(url)
  if (length(query) > 0) {
    req <- do.call(httr2::req_url_query, c(list(req), query))
  }
  if (length(headers) > 0) {
    req <- do.call(httr2::req_headers, c(list(req), as.list(headers)))
  }
  req |>
    httr2::req_error(is_error = \(r) httr2::resp_status(r) >= 400) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}

#' Perform a POST request with a JSON body and return parsed JSON
#'
#' @param url Character. The request URL.
#' @param body Named list to serialise as the JSON request body.
#' @param headers Named character vector of additional HTTP headers.
#'   \code{Content-Type: application/json} is always set automatically.
#'
#' @return Parsed JSON response as a list.
#' @keywords internal
http_post_json <- function(url, body, headers = character()) {
  req <- httr2::request(url)
  if (length(headers) > 0) {
    req <- do.call(httr2::req_headers, c(list(req), as.list(headers)))
  }
  req |>
    httr2::req_body_json(body) |>
    httr2::req_error(is_error = \(r) httr2::resp_status(r) >= 400) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = FALSE)
}
