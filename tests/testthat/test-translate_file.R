mock_translate <- function() {
  calls <- character(0)
  fn <- function(text, target_language = "en", source_language = "auto") {
    calls <<- c(calls, text)
    if (identical(text, "FAIL")) return(invisible(NULL))
    paste0("<", target_language, ":", text, ">")
  }
  list(fn = fn, calls = function() calls)
}

test_that("overwrite = TRUE writes the translated lines back to the file", {
  m <- mock_translate()
  local_mocked_bindings(google_translate = m$fn)
  path <- tempfile(fileext = ".txt")
  writeLines(c("Hello", "World"), path)

  out <- translate_file(path, "fr", "en", overwrite = TRUE)
  expect_equal(out, path)
  expect_equal(readLines(path), c("<fr:Hello>", "<fr:World>"))
})

test_that("overwrite = FALSE writes a new file and returns its path invisibly", {
  m <- mock_translate()
  local_mocked_bindings(google_translate = m$fn)
  dir <- tempfile("dir"); dir.create(dir)
  path <- file.path(dir, "notes.txt")
  writeLines("Hello", path)

  expect_invisible(out <- translate_file(path, "de"))
  expect_equal(out, file.path(dir, "notes_de_translated.txt"))
  expect_equal(readLines(out), "<de:Hello>")
  expect_equal(readLines(path), "Hello")
})

test_that("indentation is preserved once and not sent to the service", {
  m <- mock_translate()
  local_mocked_bindings(google_translate = m$fn)
  path <- tempfile(fileext = ".txt")
  writeLines(c("  Hello", "\tTab"), path)

  translate_file(path, "fr", overwrite = TRUE)
  expect_equal(readLines(path), c("  <fr:Hello>", "\t<fr:Tab>"))
  expect_equal(m$calls(), c("Hello", "Tab"))
})

test_that("blank lines are kept and not sent to the service", {
  m <- mock_translate()
  local_mocked_bindings(google_translate = m$fn)
  path <- tempfile(fileext = ".txt")
  writeLines(c("Hello", "", "   ", "World"), path)

  translate_file(path, "fr", overwrite = TRUE)
  expect_equal(readLines(path), c("<fr:Hello>", "", "   ", "<fr:World>"))
  expect_equal(m$calls(), c("Hello", "World"))
})

test_that("untranslatable lines are kept and reported in a warning", {
  m <- mock_translate()
  local_mocked_bindings(google_translate = m$fn)
  path <- tempfile(fileext = ".txt")
  writeLines(c("Hello", "FAIL", "  FAIL", "World"), path)

  expect_warning(
    translate_file(path, "fr", overwrite = TRUE),
    "2 line\\(s\\) could not be translated"
  )
  expect_equal(readLines(path), c("<fr:Hello>", "FAIL", "  FAIL", "<fr:World>"))
})

test_that("roxygen lines keep their marker; failures leave the line intact", {
  m <- mock_translate()
  local_mocked_bindings(google_translate = m$fn)
  path <- tempfile(fileext = ".R")
  writeLines(c("#' Title here", "#'", "#' FAIL", "  #' Indented"), path)

  expect_warning(translate_file(path, "nl", overwrite = TRUE), "1 line")
  expect_equal(
    readLines(path),
    c("#' <nl:Title here>", "#'", "#' FAIL", "  #' <nl:Indented>")
  )
  expect_equal(m$calls(), c("Title here", "FAIL", "Indented"))
})
