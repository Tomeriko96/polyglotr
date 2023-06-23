test_that("language detection works", {
  expect_equal(language_detect("hello"), "en")
  expect_equal(language_detect("bongiorno"), "it")
  expect_equal(language_detect("cerveza"), "es")
  expect_equal(language_detect("madelief"), "nl")
  expect_equal(language_detect("unwort"), "de")
})
