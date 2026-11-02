# NHL fixtures

`xg_two_games.rds`: a list of two data frames, the `nhl_game_pbp()` output for
games 2025021229 (TBL at BUF) and 2025021230, captured from the public NHL API
(`api-web.nhle.com`) on 2026-11-02 and cut to the 21 columns
`helper_nhl_calculate_xg()` reads (the `xg` column is dropped). Used by
`test-calculate_xg.R` to check that a multi-game frame keeps one row per event.
