test_that("batch_translate writes one translated file per target language", {
  local_mocked_bindings(
    google_translate = function(text, target_language = "en", source_language = "auto") {
      paste0("<", source_language, ">", target_language, ":", text)
    }
  )
  dir <- tempfile("batch"); dir.create(dir)
  input <- file.path(dir, "README.md")
  writeLines(c("Hallo", "  Wereld"), input)

  expect_invisible(paths <- batch_translate(input, "nl", c("fr", "es")))
  expect_equal(
    unname(paths),
    file.path(dir, c("README_fr_translated.md", "README_es_translated.md"))
  )
  expect_equal(names(paths), c("fr", "es"))
  expect_equal(readLines(paths[["fr"]]), c("<nl>fr:Hallo", "  <nl>fr:Wereld"))
  expect_equal(readLines(paths[["es"]]), c("<nl>es:Hallo", "  <nl>es:Wereld"))
  expect_equal(readLines(input), c("Hallo", "  Wereld"))
})

test_that("batch_translate never sends the file path as text", {
  sent <- character(0)
  local_mocked_bindings(
    google_translate = function(text, target_language = "en", source_language = "auto") {
      sent <<- c(sent, text)
      text
    }
  )
  input <- tempfile(fileext = ".txt")
  writeLines("content", input)
  batch_translate(input, "en", "de")
  expect_equal(sent, "content")
})

test_that("batch_translate errors on a missing input file", {
  expect_error(batch_translate(tempfile(), "en", "fr"), "does not exist")
})

test_that("batch_translate validates all languages before writing anything", {
  local_mocked_bindings(
    google_translate = function(text, target_language = "en", source_language = "auto") text
  )
  dir <- tempfile("batch"); dir.create(dir)
  input <- file.path(dir, "doc.md")
  writeLines("Hallo", input)
  expect_error(batch_translate(input, "nl", c("de", "xx", "fr")), "Invalid language code\\(s\\): xx")
  expect_error(batch_translate(input, "zz", "de"), "zz")
  expect_equal(list.files(dir), "doc.md")
})
