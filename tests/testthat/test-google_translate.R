test_that("translation works", {
  expect_equal(google_translate("hello",
                                target_language = "es"),
               "Hola")
})
