test_that("invalid target language code errors", {
  expect_error(google_translate("Hello", target_language = "xyz123"))
})

test_that("invalid source language code errors", {
  expect_error(google_translate("Hello", target_language = "en", source_language = "xyz123"))
})

test_that("short text translates correctly", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  result <- google_translate("Hello", target_language = "es", source_language = "en")
  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(nchar(result) > 0)
})

test_that("vector of short texts translates correctly", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  result <- google_translate(c("Hello", "World"), target_language = "es", source_language = "en")
  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(nchar(result) > 0))
})

test_that("long text returns non-empty translation, not character(0) — issue #13", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  # Reproducer from issue #13: Bulgarian text that exceeds the Google Translate
  # mobile endpoint URL limit (~1500+ Cyrillic chars → URL too long → character(0)).
  # Each Cyrillic character encodes to 6 URL chars (%XX%XX), so 1500 chars → ~9000
  # chars in the URL, which triggers an empty response from Google's mobile endpoint.
  phrase <- paste(
    "Гордеем се да бъдем стабилен и предпочитан работодател в региона грижещ се за",
    "безопасността и благополучието на своите работници и служители.",
    "Ангажираността на нашите колеги и техният непрекъснат стремеж към съвършенство",
    "осигуряват растежа ни и признанието което получаваме от нашите клиенти."
  )
  long_text <- paste(rep(phrase, 15), collapse = " ")

  expect_true(nchar(long_text) > 4000)

  result <- google_translate(long_text, target_language = "en", source_language = "auto")

  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(nchar(result) > 0)
  expect_false(identical(result, character(0)))
})

test_that("vector containing a long text translates correctly", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  long_text <- paste(rep("This is a sentence that will be repeated to produce a long input text.", 20), collapse = " ")
  expect_true(nchar(long_text) > 1000)

  result <- google_translate(c(long_text, "Hello"), target_language = "de", source_language = "en")

  expect_type(result, "character")
  expect_length(result, 2)
  expect_true(all(nchar(result) > 0))
})

test_that("google_translate_long_text emits deprecation warning", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  expect_warning(
    google_translate_long_text("Hello", target_language = "es"),
    "deprecated"
  )
})
