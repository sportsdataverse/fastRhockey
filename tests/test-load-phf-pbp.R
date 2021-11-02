require(testthat)

test_that("load_phf_boxscore",
          {
            skip_on_cran()

            x <- fastRhockey::load_phf_pbp(game_id = 40863)
            y <- fastRhockey::load_phf_pbp(game_id = 268079)
            z <- fastRhockey::load_phf_pbp(game_id = 378721)

            expect_equal(colnames(x), colnames(y))
            expect_equal(colnames(x), colnames(z))
            expect_equal(colnames(y), colnames(z))
            expect_s3_class(x, "data.frame")
            expect_s3_class(y, "data.frame")
            expect_s3_class(z, "data.frame")
          })
