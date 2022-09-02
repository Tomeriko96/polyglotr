test_that("translation works", {
  expect_equal(google_translate("hello world",
                                target_language = "es"),
               "Hola Mundo")
})
