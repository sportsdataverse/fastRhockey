test_that("update_nhl_db validates force_rebuild argument", {
  skip_on_cran()
  skip_nhl_test()
  skip_if_not_installed("DBI")
  skip_if_not_installed("RSQLite")

  # Invalid force_rebuild should error
  expect_error(
    update_nhl_db(force_rebuild = "invalid_string"),
    "force_rebuild"
  )
})
