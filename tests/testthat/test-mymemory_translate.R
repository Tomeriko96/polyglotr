test_that("translation works", {
  expect_equal(
    mymemory_translate(
      text = "Hello",
      target_language = "nl",
      source_language = "en"
    ),
    "Hallo"
  )
})
