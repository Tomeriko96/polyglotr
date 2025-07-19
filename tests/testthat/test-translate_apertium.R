test_that("translation works", {
  skip_on_cran()
  skip_if_offline()
  
  result <- translate_apertium(
    text = "Hello",
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
})

test_that("function handles basic parameters", {
  # Test that function handles the correct parameters
  expect_error(translate_apertium(), "argument \"text\" is missing")
  expect_error(translate_apertium("test"), "argument \"target_language\" is missing")
  expect_error(translate_apertium("test", "es"), "argument \"source_language\" is missing")
})

test_that("function handles custom host parameter", {
  skip_on_cran()
  skip_if_offline()
  
  # Test with default host parameter
  expect_no_error(translate_apertium("Hello", "es", "en"))
  
  # Test with custom host parameter
  expect_no_error(translate_apertium("Hello", "es", "en", host = "https://apertium.org/apy"))
})