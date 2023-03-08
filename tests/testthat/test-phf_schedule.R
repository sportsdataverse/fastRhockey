test_that("phf_schedule", {
  skip_on_cran()
  cols <- c(
    "type",
    "id",
    "league_id",
    "season_id",
    "tournament_id",
    "game_id",
    "number",
    "datetime",
    "datetime_tz",
    "time_zone",
    "time_zone_abbr",
    "updated_at",
    "created_at",
    "home_team_id",
    "home_team",
    "home_team_short",
    "home_team_logo_url",
    "away_team_id",
    "away_team",
    "away_team_short",
    "away_team_logo_url",
    "home_division_id",
    "home_division",
    "away_division_id",
    "away_division",
    "home_score",
    "away_score",
    "home_shots",
    "away_shots",
    "home_penalty_minutes",
    "away_penalty_minutes",
    "home_roster_count",
    "away_roster_count",
    "facility_id",
    "facility",
    "facility_address",
    "rink_id",
    "rink",
    "game_type",
    "notes",
    "status",
    "overtime",
    "shootout",
    "allow_players",
    "tickets_url",
    "watch_live_url",
    "external_url",
    "has_play_by_play",
    "highlight_color",
    "attendance",
    "date_group",
    "winner"
  )
  x <- phf_schedule(season = 2017)
  z <- phf_schedule(season = 2023)

  expect_equal(colnames(x), cols)
  expect_equal(colnames(z), cols)

  expect_s3_class(x, "data.frame")
  expect_s3_class(z, "data.frame")
})
