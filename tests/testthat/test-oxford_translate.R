test_that("Oxford Dictionaries translation works", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Skip if no API credentials
  api_key <- Sys.getenv("OXFORD_API_KEY")
  app_id <- Sys.getenv("OXFORD_APP_ID")
  
  if (api_key == "" || app_id == "") {
    skip("Oxford Dictionaries API credentials not available")
  }
  
  # Test basic translation
  result <- oxford_translate(
    text = "hello",
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_false(result == "")
})

test_that("Oxford Dictionaries handles vector input", {
  skip_if_interactive()
  skip_if_http_error()
  
  # Skip if no API credentials
  api_key <- Sys.getenv("OXFORD_API_KEY")
  app_id <- Sys.getenv("OXFORD_APP_ID")
  
  if (api_key == "" || app_id == "") {
    skip("Oxford Dictionaries API credentials not available")
  }
  
  # Test vector translation
  result <- oxford_translate(
    text = c("hello", "world"),
    target_language = "es",
    source_language = "en"
  )
  
  expect_type(result, "character")
  expect_length(result, 2)
  expect_false(any(is.na(result)))
  expect_false(any(result == ""))
})

test_that("Oxford Dictionaries requires API credentials", {
  # Test that function fails without credentials
  expect_error(
    oxford_translate("hello", "es", "en", api_key = "", app_id = ""),
    "Oxford Dictionaries API key and App ID are required"
  )
})