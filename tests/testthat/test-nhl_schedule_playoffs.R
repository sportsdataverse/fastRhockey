test_that(".extract_series_map: handles empty/NULL carousel", {
    expect_null(fastRhockey:::.extract_series_map(NULL))
    expect_null(fastRhockey:::.extract_series_map(list()))
    expect_null(fastRhockey:::.extract_series_map(list(rounds = NULL)))
})

test_that(".extract_series_map: extracts letter and round from nested rounds", {
    # Carousel structure produced by jsonlite::fromJSON(..., flatten = TRUE):
    # $rounds is a data frame; each row has $series (a list-column of data frames).
    fake_carousel <- list(
        rounds = data.frame(
            roundNumber = c(1L, 2L),
            stringsAsFactors = FALSE
        )
    )
    fake_carousel$rounds$series <- list(
        data.frame(
            seriesLetter = c("a", "b", "c", "d"),
            stringsAsFactors = FALSE
        ),
        data.frame(
            seriesLetter = c("i", "j"),
            stringsAsFactors = FALSE
        )
    )

    out <- fastRhockey:::.extract_series_map(fake_carousel)

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 6L)
    expect_setequal(out$series_letter, c("a", "b", "c", "d", "i", "j"))
    expect_equal(
        out$playoff_round[out$series_letter == "a"], 1L
    )
    expect_equal(
        out$playoff_round[out$series_letter == "i"], 2L
    )
})

test_that(".extract_series_map: tolerates letterCode alternate field name", {
    fake_carousel <- list(
        rounds = data.frame(
            roundNumber = 1L,
            stringsAsFactors = FALSE
        )
    )
    fake_carousel$rounds$series <- list(
        data.frame(
            letterCode = c("a", "b"),
            stringsAsFactors = FALSE
        )
    )
    out <- fastRhockey:::.extract_series_map(fake_carousel)
    expect_equal(nrow(out), 2L)
    expect_setequal(out$series_letter, c("a", "b"))
})
