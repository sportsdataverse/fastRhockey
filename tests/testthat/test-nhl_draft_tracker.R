test_that("NHL - Get Draft Tracker (live)", {
    skip_on_cran()
    skip_nhl_test()
    x <- nhl_draft_tracker()

    # The draft tracker is only populated during an active draft window,
    # so outside that window it may legitimately return NULL.
    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_s3_class(x, "fastRhockey_data")
        expect_true(nrow(x) > 0)
    }
})
