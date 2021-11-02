require(testthat)

test_that("phf_schedule",
                    {
                      skip_on_cran()

                      x <- fastRhockey::phf_schedule(season = 2017)
                      y <- fastRhockey::phf_schedule(season = 2019)
                      z <- fastRhockey::phf_schedule(season = 2021)

                      expect_equal(colnames(x), colnames(y))
                      expect_equal(colnames(x), colnames(z))
                      expect_equal(colnames(y), colnames(z))
                      expect_s3_class(x, "data.frame")
                      expect_s3_class(y, "data.frame")
                      expect_s3_class(z, "data.frame")
                    })
