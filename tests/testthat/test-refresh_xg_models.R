test_that("refresh_xg_models requires xgboost", {
    skip_on_cran()
    skip_nhl_test()
    skip_if_not_installed("xgboost")
    # Just verify it runs without error
    expect_no_error(refresh_xg_models())
})
