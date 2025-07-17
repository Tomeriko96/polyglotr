test_that("Microsoft Translator translation works", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Test basic translation
  result <- microsoft_translate(
    text = "Hello",
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_false(result == "")
})

test_that("Microsoft Translator handles vector input", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Test vector translation
  result <- microsoft_translate(
    text = c("Hello", "World"),
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_length(result, 2)
  expect_false(any(is.na(result)))
  expect_false(any(result == ""))
})

test_that("Microsoft Translator handles auto-detection", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Test auto-detection
  result <- microsoft_translate(
    text = "Hello",
    target_language = "es",
    source_language = "auto"
  )
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_false(result == "")
})