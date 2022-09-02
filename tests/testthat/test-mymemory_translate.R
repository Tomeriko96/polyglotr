test_that("translation workds", {
  expect_equal(mymemory_translate(text = "Hello World",
                                  target_language = "es",
                                  source_language = "en"),
               "Hola Mundo")
})
