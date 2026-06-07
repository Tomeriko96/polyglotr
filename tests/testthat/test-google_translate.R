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

  # Reproducer from issue #13 scaled to 30,000+ characters — far beyond any
  # plausible URL limit expansion. Each Cyrillic char URL-encodes to 6 ASCII chars
  # (%D0%XX), so 30k chars → ~180k-char URL. No server accepts that; chunking
  # is the only way this can succeed.
  phrase <- paste(
    "Гордеем се да бъдем стабилен и предпочитан работодател в региона грижещ се за",
    "безопасността и благополучието на своите работници и служители.",
    "Ангажираността на нашите колеги и техният непрекъснат стремеж към съвършенство",
    "осигуряват растежа ни и признанието което получаваме от нашите клиенти."
  )
  long_text <- paste(rep(phrase, 200), collapse = " ")

  # Confirm the URL this would produce is absurdly long without chunking
  url_encoded_len <- nchar(urltools::url_encode(long_text))
  expect_true(nchar(long_text) > 30000)
  expect_true(url_encoded_len > 100000)

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
