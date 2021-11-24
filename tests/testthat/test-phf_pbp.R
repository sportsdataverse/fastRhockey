test_that("phf_pbp", {
  skip_on_cran()

  x <- fastRhockey::phf_pbp(game_id = 40863)
  y <- fastRhockey::phf_pbp(game_id = 268079)
  z <- fastRhockey::phf_pbp(game_id = 378721)

  expect_equal(colnames(x), colnames(y))
  expect_equal(colnames(x), colnames(z))
  expect_equal(colnames(y), colnames(z))
  expect_s3_class(x, "data.frame")
  expect_s3_class(y, "data.frame")
  expect_s3_class(z, "data.frame")
})
