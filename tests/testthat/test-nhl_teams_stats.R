test_that("NHL - Get NHL Teams Stats", {
  skip_on_cran()
  x <- nhl_teams_stats(team_id=14)
  
  cols <- c("splits_stat_games_played",
            "splits_stat_wins",
            "splits_stat_losses",
            "splits_stat_ot",
            "splits_stat_pts",
            "splits_stat_pt_pctg",
            "splits_stat_goals_per_game",
            "splits_stat_goals_against_per_game",
            "splits_stat_ev_gga_ratio",
            "splits_stat_power_play_percentage",
            "splits_stat_power_play_goals",
            "splits_stat_power_play_goals_against",
            "splits_stat_power_play_opportunities",
            "splits_stat_penalty_kill_percentage",
            "splits_stat_shots_per_game",
            "splits_stat_shots_allowed",
            "splits_stat_win_score_first",
            "splits_stat_win_opp_score_first",
            "splits_stat_win_lead_first_per",
            "splits_stat_win_lead_second_per",
            "splits_stat_win_outshoot_opp",
            "splits_stat_win_outshot_by_opp",
            "splits_stat_face_offs_taken",
            "splits_stat_face_offs_won",
            "splits_stat_face_offs_lost",
            "splits_stat_face_off_win_percentage",
            "splits_stat_shooting_pctg",
            "splits_stat_save_pctg",
            "splits_team_id",
            "splits_team_name",
            "splits_team_link",
            "splits_stat_penalty_kill_opportunities",
            "splits_stat_save_pct_rank",
            "splits_stat_shooting_pct_rank",
            "type_display_name",
            "type_game_type_id",
            "type_game_type_description",
            "type_game_type_postseason")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')
  
})
