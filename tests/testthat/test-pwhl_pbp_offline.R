.load_fx <- function(stem) {
  jsonlite::read_json(testthat::test_path("fixtures", "hockeytech",
                                          paste0(stem, ".json")))
}

test_that("pwhl_pbp pipeline (offline) yields enriched superset", {
  pbp      <- .load_fx("pwhl_pbp_42")
  df       <- fastRhockey:::.parse_hockeytech_pbp(pbp, game_id = 42)
  out      <- fastRhockey:::hockeytech_enrich_pbp(
    df, "pwhl", 42,
    meta_payload   = .load_fx("pwhl_game_summary_42"),
    shifts_payload = .load_fx("pwhl_gameshifts_42")
  )

  for (col in c("game_id", "event", "shot_distance", "scoring_chance",
                "on_ice_home", "home_team_id")) {
    expect_true(col %in% names(out), info = col)
  }

  expect_true(any(out$event == "blocked_shot"))

  shots <- out[!is.na(out$event) & out$event == "shot" &
               !is.na(out$on_ice_home), ]
  expect_true(nrow(shots) > 0 && grepl(",", shots$on_ice_home[1]))
})
