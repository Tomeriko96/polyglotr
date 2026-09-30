test_that("translation_table builds one column per language", {
  out <- translation_table(c("a", "b"), c("x", "y"), function(w, l) paste0(w, "-", l))
  expect_equal(names(out), c("original_word", "x", "y"))
  expect_equal(out$x, c("a-x", "b-x"))
  expect_equal(out$y, c("a-y", "b-y"))
})

test_that("translation_table maps NULL to NA and keeps the first element", {
  out <- translation_table(c("a", "b"), "x", function(w, l) {
    if (w == "a") NULL else c("first", "second")
  })
  expect_equal(out$x, c(NA_character_, "first"))
})

test_that("create_translation_table uses google_translate", {
  local_mocked_bindings(
    google_translate = function(text, target_language = "en", source_language = "auto") {
      paste0(target_language, ":", text)
    }
  )
  out <- create_translation_table(c("Hello", "Table"), c("es", "fr"))
  expect_s3_class(out, "data.frame")
  expect_equal(out$es, c("es:Hello", "es:Table"))
  expect_equal(out$fr, c("fr:Hello", "fr:Table"))
})

test_that("create_transliteration_table uses the first google_transliterate candidate", {
  local_mocked_bindings(
    google_transliterate = function(text, language_tag = "el", num = 5) {
      c(paste0(language_tag, ":", text), "other")
    }
  )
  out <- create_transliteration_table(c("Hello", "Bye"), c("ru", "ar"))
  expect_equal(out$ru, c("ru:Hello", "ru:Bye"))
  expect_equal(out$ar, c("ar:Hello", "ar:Bye"))
})
