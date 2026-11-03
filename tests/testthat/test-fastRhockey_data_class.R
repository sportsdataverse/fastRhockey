# A "data.table" class after "tbl_df" sends tail() to data.table's method,
# whose x[i] then hits tibble's `[`, a column selector: "Can't subset columns
# past the end" (hoopR #88). fastRhockey_data objects must not carry it.

test_that("make_fastRhockey_data() output supports head()/tail()/print()", {
  out <- make_fastRhockey_data(data.frame(a = 1:10, b = letters[1:10]), "unit test data", Sys.time())

  expect_s3_class(out, "fastRhockey_data")
  expect_false(inherits(out, "data.table"))
  expect_equal(tail(out)$a, 5:10)
  expect_equal(head(out, 3)$a, 1:3)
  expect_no_error(suppressMessages(capture.output(print(out))))
})

test_that("every exported load_*() result supports head()/tail() (offline)", {
  fake <- function(...) data.table::data.table(a = 1:10, b = letters[1:10])
  local_mocked_bindings(
    rds_from_url = fake, csv_from_url = fake,
    # the default-season HEAD check on the newest release file
    .release_asset_exists = function(...) TRUE
  )

  # load_phf_*() are defunct (lifecycle::deprecate_stop(), the PHF folded)
  loaders <- grep("^load_(?!phf_)", getNamespaceExports("fastRhockey"), value = TRUE, perl = TRUE)
  expect_gt(length(loaders), 50)
  for (f in loaders) {
    x <- suppressMessages(getExportedValue("fastRhockey", f)())
    expect_s3_class(x, "fastRhockey_data")
    expect_false(inherits(x, "data.table"), label = f)
    expect_equal(nrow(tail(x)), 6L, label = f)
    expect_equal(nrow(head(x, 3)), 3L, label = f)
  }
})
