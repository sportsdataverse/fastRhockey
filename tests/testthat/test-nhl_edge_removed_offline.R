# The NHL removed these Edge top-10 leaderboards: each wrapper warns and returns
# NULL without sending a request.
test_that("removed NHL Edge top-10 wrappers warn and return NULL", {
  skip_on_cran()
  calls <- list(
    function() nhl_edge_goalie_5v5_top_10(sort_by = "savePctg"),
    function() nhl_edge_goalie_edge_save_pctg_top_10(sort_by = "savePctg"),
    function() nhl_edge_goalie_shot_location_top_10(category = "high", sort_by = "savePctg"),
    function() nhl_edge_skater_shot_location_top_10(position = "F", category = "high", sort_by = "total"),
    function() nhl_edge_skater_shot_speed_top_10(positions = "F", sort_by = "total"),
    function() nhl_edge_skater_speed_top_10(positions = "F", sort_by = "total"),
    function() nhl_edge_skater_zone_time_top_10(positions = "F", strength = "all", sort_by = "total"),
    function() nhl_edge_team_shot_location_top_10(position = "F", category = "high", sort_by = "total"),
    function() nhl_edge_team_skating_speed_top_10(positions = "F", sort_by = "total")
  )
  withr::local_options(lifecycle_verbosity = "warning")
  for (f in calls) {
    expect_warning(x <- f(), class = "lifecycle_warning_deprecated")
    expect_null(x)
  }
})
