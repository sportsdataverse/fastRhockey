test_that("NHL - Get Game PBP (v1 wrapper)", {
    skip_on_cran()
    skip_nhl_test()
    x <- suppressWarnings(nhl_game_pbp(game_id = 2023020001))

    if (!is.null(x)) {
        expect_s3_class(x, "data.frame")
        expect_true(nrow(x) > 0)
    }
})

test_that("NHL - nhl_game_pbp fills game_id on shift CHANGE rows (offline)", {
    # Game 2025021229: the play-by-play JSON and nhl_game_shifts() output,
    # captured from the NHL API, stand in for the two network calls.
    pbp_json <- readRDS(test_path("fixtures", "nhl", "pbp_2025021229.rds"))
    shifts <- readRDS(test_path("fixtures", "nhl", "shifts_2025021229.rds"))
    local_mocked_bindings(
        .retry_request = function(...) NULL,
        check_status = function(...) invisible(NULL),
        .resp_text = function(...) pbp_json,
        nhl_game_shifts = function(...) shifts
    )

    pbp <- suppressMessages(nhl_game_pbp(game_id = 2025021229))
    expect_s3_class(pbp, "data.frame")
    expect_s3_class(pbp, "fastRhockey_data")

    # Every captured shift row made it in, so integration really ran.
    expect_equal(sum(pbp$event_type == "CHANGE"), nrow(shifts))
    expect_false(anyNA(pbp$game_id))
    expect_true(all(pbp$game_id == 2025021229L))
    expect_type(pbp$game_id, "integer")
})

test_that("NHL - nhl_game_pbp rotates the rink by homeTeamDefendingSide (offline)", {
    pbp_json <- readRDS(test_path("fixtures", "nhl", "pbp_2025021229.rds"))
    shifts <- readRDS(test_path("fixtures", "nhl", "shifts_2025021229.rds"))
    local_mocked_bindings(
        .retry_request = function(...) NULL,
        check_status = function(...) invisible(NULL),
        .resp_text = function(...) pbp_json,
        nhl_game_shifts = function(...) shifts
    )
    pbp <- suppressMessages(nhl_game_pbp(game_id = 2025021229))
    expect_s3_class(pbp, "data.frame")
    expect_s3_class(pbp, "fastRhockey_data")
    expect_true(all(c("x", "y", "x_fixed", "y_fixed", "event_team_abbr", "home_abbr",
                      "shot_distance", "period", "period_time") %in% names(pbp)))
    plays <- jsonlite::fromJSON(pbp_json, flatten = TRUE)$plays
    side <- plays[!is.na(plays$homeTeamDefendingSide) & !is.na(plays$details.xCoord), ]
    key <- paste(side$periodDescriptor.number, side$timeInPeriod, side$details.xCoord, side$details.yCoord)
    side <- side[!duplicated(key) & !duplicated(key, fromLast = TRUE), ]
    pk <- paste(pbp$period, pbp$period_time, pbp$x, pbp$y)
    sk <- paste(side$periodDescriptor.number, side$timeInPeriod, side$details.xCoord, side$details.yCoord)
    m <- match(sk, pk)
    keep <- !is.na(m)
    expect_gt(sum(keep), 150)
    flip <- side$homeTeamDefendingSide[keep] == "right"
    # every event: home defends left -> (x, y) kept; right -> (-x, -y)
    expect_equal(pbp$x_fixed[m[keep]], ifelse(flip, -pbp$x[m[keep]], pbp$x[m[keep]]))
    expect_equal(pbp$y_fixed[m[keep]], ifelse(flip, -pbp$y[m[keep]], pbp$y[m[keep]]))

    home <- pbp$home_abbr[1]
    goals <- pbp[pbp$event_type == "GOAL" & !is.na(pbp$x_fixed), ]
    expect_true(all(ifelse(goals$event_team_abbr == home, goals$x_fixed > 0, goals$x_fixed < 0)))
    # a blocked shot (event team = blocker) is measured to the shooter's target net
    blocked <- pbp[pbp$event_type == "BLOCKED_SHOT" & !is.na(pbp$shot_distance), ]
    expect_gt(nrow(blocked), 5)
    expect_lt(stats::median(blocked$shot_distance), 89)
})
