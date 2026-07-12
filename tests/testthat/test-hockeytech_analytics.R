test_that("shot distance/angle on a known point", {
  df <- data.frame(event = "shot", x_coord = 25, y_coord = 0)
  out <- fastRhockey:::hockeytech_shot_distance_angle(df, goal_x = 89)
  expect_equal(out$shot_distance[1], 64)
  expect_equal(out$shot_angle[1], 0)
})

test_that("scoring chance flags close shots", {
  df <- data.frame(event = c("shot", "shot"), x_coord = c(80, 10), y_coord = c(2, 2))
  out <- fastRhockey:::hockeytech_scoring_chances(fastRhockey:::hockeytech_shot_distance_angle(df))
  expect_true(out$scoring_chance[1])
  expect_false(out$scoring_chance[2])
})

test_that("coord helpers tolerate NA/character coords", {
  df <- data.frame(event = c("faceoff", "shot"), x_coord = c(NA, NA), y_coord = c(NA, NA))
  out <- fastRhockey:::hockeytech_shot_distance_angle(df)
  expect_true("shot_distance" %in% names(out))
})

test_that("player TOI sums shift lengths (countdown)", {
  shifts <- data.frame(
    player_id  = c(1, 1, 2),
    first_name = c("A", "A", "B"),
    last_name  = c("X", "X", "Y"),
    period     = c(1, 1, 1),
    start_s    = c(1200, 1100, 1200),
    end_s      = c(1180, 1090, 1150)
  )
  out <- fastRhockey:::hockeytech_player_toi(shifts)
  a <- out[out$player_id == 1, ]
  expect_equal(a$toi_seconds, 30)
  expect_equal(a$num_shifts, 2)
})

test_that("on-ice interval match + home/away split, integer ids", {
  pbp <- data.frame(event = "shot", period_of_game = 2L, time_s = 600L, team_id = 10L)
  shifts <- data.frame(
    player_id = c(11, 12, 21),
    home      = c(1, 1, 0),
    period    = c(2, 2, 2),
    start_s   = c(700, 700, 700),
    end_s     = c(500, 500, 500)
  )
  out <- fastRhockey:::hockeytech_build_on_ice(pbp, shifts)
  expect_equal(out$on_ice_home[1], "11,12")
  expect_equal(out$on_ice_away[1], "21")
})

test_that("on-ice line-change boundary is not double-counted", {
  # Outgoing shift ends exactly at the event time; incoming shift starts exactly
  # at it. End boundary is EXCLUSIVE, so only the incoming player (2) is on ice --
  # a closed interval counted both lines (impossible ~10-skater states).
  pbp <- data.frame(event = "faceoff", period_of_game = 1L, time_s = 1000L, team_id = 10L)
  shifts <- data.frame(
    player_id = c(1, 2),
    home      = c(1, 1),
    period    = c(1, 1),
    start_s   = c(1200, 1000),
    end_s     = c(1000, 800)
  )
  out <- fastRhockey:::hockeytech_build_on_ice(pbp, shifts)
  expect_equal(out$on_ice_home[1], "2")
})

test_that("strength_state: even / power play / pulled goalie / null", {
  pbp <- data.frame(
    event       = c("shot", "shot", "shot", "shot"),
    on_ice_home = c("1,2,3,4,5,99", "1,2,3,4,5,99", "1,2,3,4,5,6", NA),
    on_ice_away = c("6,7,8,9,10,88", "6,7,8,9,88", "7,8,9,10,11,88", "6,7,8,9,10,88"),
    stringsAsFactors = FALSE
  )
  out <- fastRhockey:::hockeytech_add_strength_state(pbp, goalie_ids = c("99", "88"))
  expect_equal(out$strength_state[1], "5v5")
  expect_equal(out$skaters_home[1], 5)
  expect_equal(out$skaters_away[1], 5)
  expect_true(out$strength_state_valid[1])
  expect_equal(out$strength_state[2], "5v4")            # home power play
  expect_equal(out$strength_state[3], "6v5")            # home pulled goalie -> 6 skaters
  expect_true(out$strength_state_valid[3])              # pulled goalie is a valid 6-skater state
  expect_true(is.na(out$strength_state[4]))             # null on-ice
})

test_that("strength_state flags impossible counts (boundary noise)", {
  pbp <- data.frame(
    on_ice_home = "1,2,3,4,5,6,7,99", on_ice_away = "10,11,12,13,14,88",
    stringsAsFactors = FALSE
  )
  out <- fastRhockey:::hockeytech_add_strength_state(pbp, goalie_ids = c("99", "88"))
  expect_equal(out$skaters_home[1], 7)
  expect_false(out$strength_state_valid[1])
})

test_that("strength_state without goalie_ids assumes one goalie", {
  pbp <- data.frame(on_ice_home = "1,2,3,4,5,99", on_ice_away = "6,7,8,9,10,88", stringsAsFactors = FALSE)
  out <- fastRhockey:::hockeytech_add_strength_state(pbp)
  expect_equal(out$skaters_home[1], 5)
  expect_equal(out$skaters_away[1], 5)
})

test_that("team corsi/fenwick proxies + flag", {
  pbp <- data.frame(
    event   = c("shot", "blocked_shot", "goal", "faceoff", "shot"),
    team_id = c(10, 10, 20, 10, 20)
  )
  out <- fastRhockey:::hockeytech_corsi_fenwick(pbp)
  expect_false(any(out$corsi_includes_missed))
  expect_equal(out$corsi_for[out$team_id == 10], 2)
  expect_equal(out$fenwick_for[out$team_id == 10], 1)
})

test_that("player on-ice corsi attribution", {
  pbp <- data.frame(
    event        = c("shot", "blocked_shot", "goal"),
    team_id      = c("3", "3", "1"),
    home_team_id = c("3", "3", "3"),
    on_ice_home  = c("10,11", "10,11", "10,12"),
    on_ice_away  = c("20,21", "20,21", "20,22"),
    stringsAsFactors = FALSE
  )
  out <- fastRhockey:::hockeytech_corsi_fenwick_on_ice(pbp)
  p10 <- out[out$player_id == "10", ]
  expect_equal(p10$corsi_for, 2)
  expect_equal(p10$corsi_against, 1)
  p20 <- out[out$player_id == "20", ]
  expect_equal(p20$corsi_for, 1)
  expect_equal(p20$corsi_against, 2)
})
