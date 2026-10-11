# Offline: every NHL release loader accepts the first season its release
# publishes and rejects the season before it, without touching the network.
# The floors are the first `<file_prefix>_<year>.rds` asset on each
# sportsdataverse-data release tag (2009-10, i.e. 2010, for all but pbp_lite,
# whose `play_by_play_lite_<year>.rds` files start in 2024).

nhl_release_floors <- list(
  load_nhl_pbp             = list("nhl_pbp_full",         "play_by_play",      2010),
  load_nhl_pbp_lite        = list("nhl_pbp_lite",         "play_by_play_lite", 2024),
  load_nhl_team_box        = list("nhl_team_boxscores",   "team_box",          2010),
  load_nhl_player_box      = list("nhl_player_boxscores", "player_box",        2010),
  load_nhl_skater_box      = list("nhl_skater_boxscores", "skater_box",        2010),
  load_nhl_goalie_box      = list("nhl_goalie_boxscores", "goalie_box",        2010),
  load_nhl_schedule        = list("nhl_schedules",        "nhl_schedule",      2010),
  load_nhl_rosters         = list("nhl_rosters",          "rosters",           2010),
  load_nhl_game_rosters    = list("nhl_game_rosters",     "game_rosters",      2010),
  load_nhl_game_info       = list("nhl_game_info",        "game_info",         2010),
  load_nhl_scoring         = list("nhl_scoring",          "scoring",           2010),
  load_nhl_penalties       = list("nhl_penalties",        "penalties",         2010),
  load_nhl_three_stars     = list("nhl_three_stars",      "three_stars",       2010),
  load_nhl_scratches       = list("nhl_scratches",        "scratches",         2010),
  load_nhl_linescore       = list("nhl_linescore",        "linescore",         2010),
  load_nhl_shifts          = list("nhl_shifts",           "shifts",            2010),
  load_nhl_officials       = list("nhl_officials",        "officials",         2010),
  load_nhl_shots_by_period = list("nhl_shots_by_period",  "shots_by_period",   2010),
  load_nhl_shootout        = list("nhl_shootout",         "shootout_summary",  2010)
)

test_that("NHL release loaders start at their first published season", {
  skip_on_cran()
  seen <- character()
  local_mocked_bindings(rds_from_url = function(url) {
    seen <<- c(seen, url)
    data.table::data.table(season = 1L)
  })
  for (fn in names(nhl_release_floors)) {
    spec <- nhl_release_floors[[fn]]
    loader <- getExportedValue("fastRhockey", fn)
    first <- spec[[3]]

    out <- loader(first)
    expect_s3_class(out, "fastRhockey_data")
    expect_identical(
      seen[length(seen)],
      paste0("https://github.com/sportsdataverse/sportsdataverse-data/releases/download/",
             spec[[1]], "/", spec[[2]], "_", first, ".rds"),
      info = fn
    )

    n <- length(seen)
    expect_error(loader(first - 1), info = fn)
    expect_identical(length(seen), n, info = paste(fn, "fetched before rejecting"))
  }
})
