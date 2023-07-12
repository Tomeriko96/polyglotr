test_that("translation works", {
  expect_equal(
    google_translate("hello",
      target_language = "es"
    ),
    "Hola"
  )
})

# Unit tests for vectorized input
test_that("google_translate returns correct translations for vectorized input", {
  text_to_translate <- c("the", "quick", "brown")
  translations <- google_translate(text_to_translate, target_language = "fr", source_language = "en")

  expected_translations <- list("le", "rapide", "brun")
  expect_equal(translations, expected_translations)
})

# Unit tests for unvectorized input
test_that("google_translate returns correct translation for unvectorized input", {
  text_to_translate <- "I love languages"
  translation <- google_translate(text_to_translate, target_language = "es")

  expected_translation <- "me encantan los idiomas"
  expect_equal(translation, expected_translation)
})
