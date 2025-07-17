test_that("pons_dictionary_search works correctly", {
  # Test basic functionality
  result <- pons_dictionary_search("hello", "en", "es", limit = 3)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= 3)
  
  if (nrow(result) > 0) {
    expect_true(all(c("source_word", "word_class", "translations", "source_language", "target_language") %in% names(result)))
    expect_equal(result$source_language[1], "en")
    expect_equal(result$target_language[1], "es")
  }
})

test_that("pons_dictionary_search handles missing parameters", {
  expect_error(pons_dictionary_search(), "query, source_language, and target_language are required parameters")
  expect_error(pons_dictionary_search("hello"), "query, source_language, and target_language are required parameters")
  expect_error(pons_dictionary_search("hello", "en"), "query, source_language, and target_language are required parameters")
})

test_that("pons_word_examples works correctly", {
  # Test basic functionality
  result <- pons_word_examples("hello", "en", "es", limit = 2)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= 2)
  
  if (nrow(result) > 0) {
    expect_true(all(c("source_example", "target_example", "source_language", "target_language") %in% names(result)))
    expect_equal(result$source_language[1], "en")
    expect_equal(result$target_language[1], "es")
  }
})

test_that("pons_word_examples handles missing parameters", {
  expect_error(pons_word_examples(), "word, source_language, and target_language are required parameters")
  expect_error(pons_word_examples("hello"), "word, source_language, and target_language are required parameters")
  expect_error(pons_word_examples("hello", "en"), "word, source_language, and target_language are required parameters")
})

test_that("pons_word_definitions works correctly", {
  # Test basic functionality
  result <- pons_word_definitions("hello", "en", "es", limit = 2)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= 2)
  
  if (nrow(result) > 0) {
    expect_true(all(c("word", "part_of_speech", "definition", "context", "source_language", "target_language") %in% names(result)))
    expect_equal(result$source_language[1], "en")
    expect_equal(result$target_language[1], "es")
  }
})

test_that("pons_word_definitions handles missing parameters", {
  expect_error(pons_word_definitions(), "word, source_language, and target_language are required parameters")
  expect_error(pons_word_definitions("hello"), "word, source_language, and target_language are required parameters")
  expect_error(pons_word_definitions("hello", "en"), "word, source_language, and target_language are required parameters")
})

test_that("pons_word_synonyms works correctly", {
  # Test basic functionality
  result <- pons_word_synonyms("hello", "en", "es", limit = 2)
  
  expect_true(is.data.frame(result))
  expect_true(nrow(result) <= 2)
  
  if (nrow(result) > 0) {
    expect_true(all(c("word", "synonym", "source_language", "target_language") %in% names(result)))
    expect_equal(result$word[1], "hello")
    expect_equal(result$source_language[1], "en")
    expect_equal(result$target_language[1], "es")
  }
})

test_that("pons_word_synonyms handles missing parameters", {
  expect_error(pons_word_synonyms(), "word, source_language, and target_language are required parameters")
  expect_error(pons_word_synonyms("hello"), "word, source_language, and target_language are required parameters")
  expect_error(pons_word_synonyms("hello", "en"), "word, source_language, and target_language are required parameters")
})

test_that("all new pons functions handle empty results gracefully", {
  # Test with a word that likely doesn't exist
  result1 <- pons_dictionary_search("zzzzxxxxxqqqqq", "en", "es")
  expect_true(is.data.frame(result1))
  
  result2 <- pons_word_examples("zzzzxxxxxqqqqq", "en", "es")
  expect_true(is.data.frame(result2))
  
  result3 <- pons_word_definitions("zzzzxxxxxqqqqq", "en", "es")
  expect_true(is.data.frame(result3))
  
  result4 <- pons_word_synonyms("zzzzxxxxxqqqqq", "en", "es")
  expect_true(is.data.frame(result4))
})

test_that("all new pons functions handle limit parameter correctly", {
  # Test with limit = 1
  result1 <- pons_dictionary_search("hello", "en", "es", limit = 1)
  expect_true(nrow(result1) <= 1)
  
  result2 <- pons_word_examples("hello", "en", "es", limit = 1)
  expect_true(nrow(result2) <= 1)
  
  result3 <- pons_word_definitions("hello", "en", "es", limit = 1)
  expect_true(nrow(result3) <= 1)
  
  result4 <- pons_word_synonyms("hello", "en", "es", limit = 1)
  expect_true(nrow(result4) <= 1)
})

test_that("pons_dictionary_search fuzzy parameter works", {
  # Test with fuzzy search enabled
  result1 <- pons_dictionary_search("helo", "en", "es", fuzzy = TRUE)
  expect_true(is.data.frame(result1))
  
  # Test with fuzzy search disabled
  result2 <- pons_dictionary_search("helo", "en", "es", fuzzy = FALSE)
  expect_true(is.data.frame(result2))
})

test_that("all new pons functions return correct column names", {
  # Test column names for dictionary search
  result1 <- pons_dictionary_search("hello", "en", "es", limit = 1)
  if (nrow(result1) > 0) {
    expected_cols1 <- c("source_word", "word_class", "translations", "source_language", "target_language")
    expect_true(all(expected_cols1 %in% names(result1)))
  }
  
  # Test column names for examples
  result2 <- pons_word_examples("hello", "en", "es", limit = 1)
  if (nrow(result2) > 0) {
    expected_cols2 <- c("source_example", "target_example", "source_language", "target_language")
    expect_true(all(expected_cols2 %in% names(result2)))
  }
  
  # Test column names for definitions
  result3 <- pons_word_definitions("hello", "en", "es", limit = 1)
  if (nrow(result3) > 0) {
    expected_cols3 <- c("word", "part_of_speech", "definition", "context", "source_language", "target_language")
    expect_true(all(expected_cols3 %in% names(result3)))
  }
  
  # Test column names for synonyms
  result4 <- pons_word_synonyms("hello", "en", "es", limit = 1)
  if (nrow(result4) > 0) {
    expected_cols4 <- c("word", "synonym", "source_language", "target_language")
    expect_true(all(expected_cols4 %in% names(result4)))
  }
})