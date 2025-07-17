test_that("DeepL translation works", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Test basic translation
  result <- deepl_translate(
    text = "Hello",
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_false(result == "")
})

test_that("DeepL handles vector input", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Test vector translation
  result <- deepl_translate(
    text = c("Hello", "World"),
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_length(result, 2)
  expect_false(any(is.na(result)))
  expect_false(any(result == ""))
})

test_that("DeepL handles auto-detection", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Test auto-detection
  result <- deepl_translate(
    text = "Hello",
    target_language = "es",
    source_language = "auto"
  )
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_false(result == "")
})