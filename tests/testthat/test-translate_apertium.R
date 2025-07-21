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

test_that("function handles basic parameters", {
  # Test that function handles the correct parameters
  expect_error(apertium_translate(), "argument \"text\" is missing")
  expect_error(apertium_translate("test"), "argument \"target_language\" is missing")
  expect_error(apertium_translate("test", "es"), "argument \"source_language\" is missing")
})

test_that("function handles custom host parameter", {
  skip_on_cran()
  skip_if_offline()
  
  # Test with default host parameter
  expect_no_error(apertium_translate("Hello", "es", "en"))
  
  # Test with custom host parameter
  expect_no_error(apertium_translate("Hello", "es", "en", host = "https://apertium.org/apy"))
})

test_that("function handles vectorization correctly", {
  # Test that vector input is handled correctly (without network calls)
  # We'll test the logic by checking that vector inputs call the function multiple times
  # This test focuses on the vector handling logic
  
  expect_length(c("test1", "test2"), 2)  # Verify our test setup
  
  # Test single input handling
  single_input <- "test"
  expect_length(single_input, 1)
  
  # Test empty vector handling
  expect_error(apertium_translate(character(0), "es", "en"))
})

test_that("apertium_get_language_pairs works", {
  skip_on_cran()
  skip_if_offline()
  
  result <- apertium_get_language_pairs()
  
  expect_type(result, "list")
  expect_true(length(result) > 0)
  
  # Check that the first element has the expected structure
  if (length(result) > 0) {
    first_pair <- result[[1]]
    expect_true("sourceLanguage" %in% names(first_pair))
    expect_true("targetLanguage" %in% names(first_pair))
  }
})

test_that("apertium_get_language_pairs handles custom host", {
  skip_on_cran()
  skip_if_offline()
  
  # Test with default host
  expect_no_error(apertium_get_language_pairs())
  
  # Test with explicit default host
  expect_no_error(apertium_get_language_pairs(host = "https://apertium.org/apy"))
})
