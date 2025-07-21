test_that("translation works", {
  skip_on_cran()
  skip_if_offline()

  result <- apertium_translate(
    text = "Hello",
    target_language = "es",
    source_language = "en"
  )

  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("vectorized translation works", {
  skip_on_cran()
  skip_if_offline()

  texts <- c("Hello", "World")
  results <- apertium_translate(
    text = texts,
    target_language = "es",
    source_language = "en"
  )

  expect_type(results, "character")
  expect_length(results, 2)
  expect_true(all(nchar(results) > 0))
})


test_that("function handles custom host parameter", {
  skip_on_cran()
  skip_if_offline()

  # Test with default host parameter
  expect_no_error(apertium_translate("Hello", "es", "en"))

  # Test with custom host parameter
  expect_no_error(apertium_translate("Hello", "es", "en", host = "https://apertium.org/apy"))
})

test_that("apertium_get_language_pairs handles custom host", {
  skip_on_cran()
  skip_if_offline()

  # Test with default host
  expect_no_error(apertium_get_language_pairs())

  # Test with explicit default host
  expect_no_error(apertium_get_language_pairs(host = "https://apertium.org/apy"))
})
