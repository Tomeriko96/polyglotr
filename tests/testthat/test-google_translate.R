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

# Unit tests for special characters from French to English
test_that("google translate returns translations with special characters", {
  text_to_translate <- "La Saône prend sa source à Vioménil dans les pré-Vosges à 405 m d'altitude. La rivière conflue avec le Rhône 473,3 km plus loin."
  translation <- google_translate(text_to_translate, target_language = "en")

  expected_translation <- "The Saône has its source at Vioménil in the pre-Vosges at an altitude of 405 m. The river confluences with the Rhône 473.3 km further."
  expect_equal(translation, expected_translation)
})


# Unit tests for special characters from Arabic to English
test_that("google translate returns translations with special characters", {
  text_to_translate <- "يتدفقُ النيل عبر الصحراء السودانية إلى مصر باتجاه الشمال ويمر في مدينةُ القاهرة الواقعة على دلتا النهر الكبيرة (دلتا النيل)، ثم يعبر النهر مدينتي دمياط ورشيد ويصب ..."
  translation <- google_translate(text_to_translate, target_language = "en")

  expected_translation <- "The Nile flows through the Sudanese desert to Egypt towards the north and passes through the city of Cairo, located on the large river delta (Nile Delta), then the river crosses the cities of Damietta and Rosetta and flows..."
  expect_equal(translation, expected_translation)
})
