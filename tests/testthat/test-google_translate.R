test_that("invalid target language code errors", {
  expect_error(google_translate("Hello", target_language = "xyz123"))
})

test_that("invalid source language code errors", {
  expect_error(google_translate("Hello", target_language = "en", source_language = "xyz123"))
})

test_that("Google JSON sentence fragments are parsed and joined", {
  response <- paste0(
    '{"sentences":[',
    '{"trans":"Hallo Welt. ","orig":"Hello world. "},',
    '{"trans":"Wie geht es dir?","orig":"How are you?"}',
    '],"src":"en"}'
  )

  expect_identical(
    polyglotr:::parse_google_translate_response(response),
    "Hallo Welt. Wie geht es dir?"
  )
})

test_that("an unexpected Google JSON response errors clearly", {
  expect_error(
    polyglotr:::parse_google_translate_response('{"src":"en"}'),
    "unexpected response"
  )
})

test_that("a Google JSON sentence without a trans field errors clearly", {
  expect_error(
    polyglotr:::parse_google_translate_response(
      '{"sentences":[{"orig":"Hello"}],"src":"en"}'
    ),
    "unexpected response"
  )
})

test_that("translations containing URLs restore original URLs", {
  replaced <- replace_urls_with_placeholders("Visit https://example.com/path?id=1 now.")
  expect_identical(replaced$text, "Visit __URL1__ now.")
  expect_identical(replaced$urls, "https://example.com/path?id=1")

  translated <- "Besuchen Sie __url1__ jetzt."
  expect_identical(
    restore_urls_from_placeholders(translated, replaced$urls),
    "Besuchen Sie https://example.com/path?id=1 jetzt."
  )
})

test_that("issue #32 reproducer translates with the JSON endpoint", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  result <- google_translate(
    "Hello, how are you?",
    target_language = "ru"
  )

  expect_type(result, "character")
  expect_length(result, 1)
  expect_true(nchar(result) > 0)
})

test_that("language detection uses the working Google client", {
  skip_on_cran()
  skip_if_offline()
  skip_if_http_error()

  expect_identical(language_detect("Bonjour tout le monde"), "fr")
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

  expect_true(nchar(long_text) > 30000)

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