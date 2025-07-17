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