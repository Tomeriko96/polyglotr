test_that("google_get_supported_languages returns the bundled snapshot", {
  langs <- google_get_supported_languages()
  expect_s3_class(langs, "data.frame")
  expect_equal(names(langs), c("Language", "ISO-639 code"))
  expect_identical(langs, polyglotr::google_supported_languages)
  expect_gt(nrow(langs), 100)
})

test_that("getter and validator agree", {
  codes <- google_get_supported_languages()$`ISO-639 code`
  expect_true(all(vapply(codes, google_is_valid_language_code, logical(1))))
  expect_true(google_is_valid_language_code("auto"))
  expect_false(google_is_valid_language_code("xx-not-a-code"))
  expect_false("xx-not-a-code" %in% codes)
})
