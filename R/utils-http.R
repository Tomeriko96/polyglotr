safe_http <- function(expr, service) {
  tryCatch(expr, error = function(e) {
    message(service, " is unavailable: ", conditionMessage(e))
    NULL
  })
}
