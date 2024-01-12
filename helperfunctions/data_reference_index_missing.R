library(purrr)
data_reference_index_missing <- function(pkg = ".", depth = 1L) {
  pkg <- pkgdown:::as_pkgdown(pkg)

  meta <- pkg$meta[["reference"]] %||% pkgdown:::default_reference_index(pkg)
  if (length(meta) == 0) {
    return(list())
  }

  # Cross-reference complete list of topics vs. topics found in index page
  all_topics <- meta %>%
    map(~ pkgdown:::select_topics(.$contents, pkg$topics)) %>%
    reduce(union)
  in_index <- seq_along(pkg$topics$name) %in% all_topics

  missing <- !in_index & !pkg$topics$internal
  pkg$topics$name[missing]
}

data_reference_index_missing()
