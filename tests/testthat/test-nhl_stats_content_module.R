test_that("NHL - Stats Content Module (smoke test only)", {
    skip_on_cran()
    skip_nhl_test()
    # `template_key` is a private NHL.com CMS identifier; we have no
    # valid value to test against, so we just smoke-test the function
    # call and tolerate either NULL (404 from CMS) or a parsed result.
    x <- nhl_stats_content_module(template_key = "example")
    expect_true(is.null(x) || inherits(x, "fastRhockey_data") || is.list(x))
})
