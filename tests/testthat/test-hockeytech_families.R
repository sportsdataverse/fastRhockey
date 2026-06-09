test_that("each junior league exposes the core + analytics surface (exported)", {
  for (lg in c("ahl","ohl","whl","qmjhl")) {
    for (stem in c("schedule","pbp","standings","teams","team_roster","player_stats",
                   "leaders","game_summary","season_id","game_shifts","player_toi","game_corsi")) {
      fn <- paste0(lg, "_", stem)
      expect_true(exists(fn, mode = "function"), info = fn)
    }
    expect_true(exists(paste0("most_recent_", lg, "_season"), mode = "function"))
  }
})
