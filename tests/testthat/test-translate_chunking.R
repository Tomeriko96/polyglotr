test_that("short text is a single chunk", {
  expect_equal(split_translation_chunks("hello world"), "hello world")
})

test_that("chunks respect max_chars and split on word boundaries", {
  text <- paste(rep("abcd", 10), collapse = " ") # 49 chars
  chunks <- split_translation_chunks(text, max_chars = 10)
  expect_true(all(nchar(chunks) <= 10))
  expect_equal(chunks[1], "abcd abcd")
  expect_equal(paste(chunks, collapse = " "), text)
})

test_that("a word longer than max_chars becomes its own chunk", {
  chunks <- split_translation_chunks("a bbbbbbbbbbbb c", max_chars = 5)
  expect_equal(chunks, c("a", "bbbbbbbbbbbb", "c"))
})

test_that("whitespace runs are collapsed on reassembly", {
  chunks <- split_translation_chunks("one  two\nthree", max_chars = 1000)
  expect_equal(chunks, "one two three")
})

test_that("default boundary of 1000 characters is kept", {
  text <- paste(rep("word", 500), collapse = " ") # 2499 chars
  chunks <- split_translation_chunks(text)
  expect_true(length(chunks) == 3)
  expect_true(all(nchar(chunks) <= 1000))
  expect_equal(paste(chunks, collapse = " "), text)
})

test_that("google_translate sends long text chunk by chunk and reassembles", {
  seen <- character(0)
  local_mocked_bindings(
    google_translate_request = function(text, target_language, source_language) {
      seen <<- c(seen, text)
      toupper(text)
    }
  )
  text <- paste(rep("word", 500), collapse = " ")
  out <- google_translate(text, "fr", "en")
  expect_length(seen, 3)
  expect_equal(out, toupper(text))
})
