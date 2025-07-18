test_that("simple_translate works with single text", {
  # Test with simple text
  result <- simple_translate("Hello", "es", "en")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(nchar(result) > 0)
})

test_that("simple_translate works with vector of texts", {
  # Test with vector of texts
  text_vector <- c("Hello", "World")
  result <- simple_translate(text_vector, "es", "en")
  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(nchar(result) > 0))
})

test_that("simple_translate handles errors gracefully", {
  # Test with invalid language codes should still return something
  result <- simple_translate("Hello", "invalid", "en")
  expect_type(result, "character")
  expect_length(result, 1)
})