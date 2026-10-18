# **Metal Ligaen Game Center**

Game-center bundle of one match as one row: game data, team stats, goal
and penalty lists.

Wraps
`GET https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/widget/esports/match/{match_id}.json`
of the Metal Ligaen eSports bucket. Example:
<https://s3.eu-west-1.amazonaws.com/den.hokejovyzapis.cz/widget/esports/match/575.json>.
Seasons are END years (2027 = 2026/27); every request waits at least one
second after the last one to the same host.

## Usage

``` r
metal_game_center(match_id, return_parsed = TRUE)
```

## Arguments

- match_id:

  Match id (per-bucket match key space: match_id in metal_schedule). Not
  the league website's game id.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON body (a nested list).

## Value

A
[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
with one row per row of the file (zero rows, carrying the documented
columns, for an empty frame); the raw JSON list when
`return_parsed = FALSE`. Columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| status_id | integer | Widget API status code, a code and not a joinable id: `1` = OK. |
| status_msg | character | Widget API status text, `Ok`. |
| data_game_data_id | integer | Match id (match key space, per bucket); joins to `league-matches` `matches[].id` and the `{match_id}` path param. |
| data_game_data_division_id | integer | League part id under the widget's name `divisionId` (league-part key space, per bucket: one stage of a competition, e.g. ICEHL 56 = 2026/27 regular season); joins to `league_part_id` in league-matches and the `{league_part_id}` path param. |
| data_game_data_division_longname | character | League part name, e.g. `ICEHL 2026/27`. |
| data_game_data_division_shortname | character | League part name, e.g. `ICEHL 2026/27`. |
| data_game_data_scheduled_date_formatted_short | character | Short display text `DD.MM.YY` (plus `HH:MM` for times). |
| data_game_data_scheduled_date_formatted_long | character | Long display text, e.g. `18.September. 2026`. |
| data_game_data_scheduled_date_timestamp | numeric | Epoch milliseconds. |
| data_game_data_scheduled_date_value | character | ISO text `YYYY-MM-DD` (plus ` HH:MM` for times). |
| data_game_data_scheduled_date_diff_to_now | integer | Difference to now as computed by the widget server; `0` in the samples. |
| data_game_data_scheduled_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
| data_game_data_start_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
| data_game_data_end_time | character | Local time `HH:MM` (scheduled face-off, actual start, actual end). |
| data_game_data_is_overtime | integer | Overtime flag sent as an integer, `1` = yes. |
| data_game_data_is_shoot_out | integer | Shootout flag sent as an integer, `1` = yes. |
| data_game_data_home_team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| data_game_data_home_team_longname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
| data_game_data_home_team_shortname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
| data_game_data_home_team_flavourname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
| data_game_data_home_team_score | integer | Final goals of the home or away team (by name). |
| data_game_data_away_team_id | integer | Team id (team key space, per bucket, stable across seasons); joins to `league-matches` `home.id` / `guest.id` and the `{team_id}` path param. |
| data_game_data_away_team_longname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
| data_game_data_away_team_shortname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
| data_game_data_away_team_flavourname | character | Team name: long, short (three-letter code) or display ('flavour') form. |
| data_game_data_away_team_score | integer | Final goals of the home or away team (by name). |
| data_game_data_game_status | integer | Game status code; `4` = finished in the samples (other codes unverified). |
| data_game_data_game_extended_status | integer | Extended status code; `0` in the samples. |
| data_game_data_game_extended_status_info_text | character | Extended status text; empty string in the samples. |
| data_game_data_game_name | character | Game number sent as a string (e.g. `"66"`). |
| data_game_data_game_day | character | Matchday number sent as a string (`"1"`); null in Denmark. |
| data_game_data_game_round | integer | Round number; `0` in the samples. |
| data_game_data_location_longname | character | Arena name; empty string when not set. |
| data_game_data_location_shortname | character | Arena name; empty string when not set. |
| data_game_data_location_coordinates_lat | integer | Latitude / longitude; `0` placeholder in the samples. |
| data_game_data_location_coordinates_lng | integer | Latitude / longitude; `0` placeholder in the samples. |
| data_game_data_attendance | integer | Spectators at the game (attendance). |
| data_game_data_live_time | integer | Game clock, seconds of game time from the opening face-off (0-3600 in regulation, continuing through overtime). 3600 at the end of regulation. |
| data_game_data_live_time_formatted | character | Clock text; the placeholder `END-GAME` after the game, `m:ss` while live. |
| data_game_data_live_time_period | integer | Current period number; `4` after the game. |
| data_game_data_live_time_string | character | Clock text; the placeholder `END-GAME` after the game, `m:ss` while live. |
| data_game_data_live_time_game_phase | character | Game phase text, e.g. `END OF GAME`. |
| data_game_data_period_stats | character | Per-period score and shots. |
| data_game_data_game_officials_ref1_lastname | character | Last name of that official or coach. |
| data_game_data_game_officials_ref1_firstname | character | First name of that official or coach. |
| data_game_data_game_officials_ref2_lastname | character | Last name of that official or coach. |
| data_game_data_game_officials_ref2_firstname | character | First name of that official or coach. |
| data_game_data_game_officials_linesman1_lastname | character | Last name of that official or coach. |
| data_game_data_game_officials_linesman1_firstname | character | First name of that official or coach. |
| data_game_data_game_officials_linesman2_lastname | character | Last name of that official or coach. |
| data_game_data_game_officials_linesman2_firstname | character | First name of that official or coach. |
| data_game_data_team_officials_headcoachhome_lastname | character | Last name of that official or coach. |
| data_game_data_team_officials_headcoachhome_firstname | character | First name of that official or coach. |
| data_game_data_team_officials_headcoachaway_lastname | character | Last name of that official or coach. |
| data_game_data_team_officials_headcoachaway_firstname | character | First name of that official or coach. |
| data_game_data_live_time_period_bar | character | Period progress bar; null in the samples, so unobserved. |
| data_game_data_you_tube_link | character | Video link; null in the samples, so unobserved. |
| data_game_data_number_of_periods | integer | Regulation periods (3) or the maximum overtimes allowed. |
| data_game_data_length_of_period | integer | Length of a period or overtime, seconds (1200). |
| data_game_data_number_of_overtimes | integer | Regulation periods (3) or the maximum overtimes allowed. |
| data_game_data_length_of_overtime | integer | Length of a period or overtime, seconds (1200). |
| data_game_data_shootout_shots | integer | Shootout attempts in the game. |
| data_game_data_images_home_team_logo | character | Team logo; null in the samples, so unobserved. |
| data_game_data_images_away_team_logo | character | Team logo; null in the samples, so unobserved. |
| data_game_data_streams | character | Video streams; empty array in the samples. |
| data_game_data_home_team_logo_url | character | Team logo URL; null in the samples, so unobserved. |
| data_game_data_away_team_logo_url | character | Team logo URL; null in the samples, so unobserved. |
| data_home_goals | character | Goals scored by that team. |
| data_home_penalties | character | Penalties of that team. |
| data_home_field_players | character | Skaters of that team with their game stats. |
| data_home_goal_keepers | character | Goalies of that team with their game stats. |
| data_home_team_stats_faceoffs_won | integer | Face-offs the team won in this game. |
| data_home_team_stats_faceoffs_total | integer | Face-offs the team took in this game. |
| data_home_team_stats_shots_on_goal | integer | Team shots that reached the goalie in this game. |
| data_home_team_stats_shots_blocked | integer | Team blocked-shot count in this game (direction unverified). |
| data_home_team_stats_penalty_minutes | integer | Penalty minutes assessed to the team in this game. |
| data_home_team_stats_powerplay_goals | integer | Goals the team scored on the power play in this game. |
| data_home_team_stats_powerplay_opportunities | integer | Power plays the team had in this game. |
| data_home_team_stats_penaltykill_successes | integer | Penalties killed without conceding. |
| data_home_team_stats_penaltykill_situations | integer | Times the team had to kill a penalty in this game. |
| data_home_team_stats_time_values_leading | integer | Seconds spent leading, tied or trailing (by name). |
| data_home_team_stats_time_values_tied | integer | Seconds spent leading, tied or trailing (by name). |
| data_home_team_stats_time_values_trailing | integer | Seconds spent leading, tied or trailing (by name). |
| data_home_team_stats_time_values_time_game_strength_even | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_home_team_stats_time_values_time_game_strength_plus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_home_team_stats_time_values_time_game_strength_plus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_home_team_stats_time_values_time_game_strength_minus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_home_team_stats_time_values_time_game_strength_minus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_home_team_stats_time_values_time_on_powerplay | integer | Seconds on the power play. |
| data_home_team_stats_time_values_time_while_shorthanded | integer | Seconds the team played shorthanded in this game. |
| data_away_goals | character | Goals scored by that team. |
| data_away_penalties | character | Penalties of that team. |
| data_away_field_players | character | Skaters of that team with their game stats. |
| data_away_goal_keepers | character | Goalies of that team with their game stats. |
| data_away_team_stats_faceoffs_won | integer | Face-offs the team won in this game. |
| data_away_team_stats_faceoffs_total | integer | Face-offs the team took in this game. |
| data_away_team_stats_shots_on_goal | integer | Team shots that reached the goalie in this game. |
| data_away_team_stats_shots_blocked | integer | Team blocked-shot count in this game (direction unverified). |
| data_away_team_stats_penalty_minutes | integer | Penalty minutes assessed to the team in this game. |
| data_away_team_stats_powerplay_goals | integer | Goals the team scored on the power play in this game. |
| data_away_team_stats_powerplay_opportunities | integer | Power plays the team had in this game. |
| data_away_team_stats_penaltykill_successes | integer | Penalties killed without conceding. |
| data_away_team_stats_penaltykill_situations | integer | Times the team had to kill a penalty in this game. |
| data_away_team_stats_time_values_leading | integer | Seconds spent leading, tied or trailing (by name). |
| data_away_team_stats_time_values_tied | integer | Seconds spent leading, tied or trailing (by name). |
| data_away_team_stats_time_values_trailing | integer | Seconds spent leading, tied or trailing (by name). |
| data_away_team_stats_time_values_time_game_strength_even | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_away_team_stats_time_values_time_game_strength_plus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_away_team_stats_time_values_time_game_strength_plus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_away_team_stats_time_values_time_game_strength_minus_one | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_away_team_stats_time_values_time_game_strength_minus_two | integer | Seconds at that skater difference (even, +1, +2, -1, -2). |
| data_away_team_stats_time_values_time_on_powerplay | integer | Seconds on the power play. |
| data_away_team_stats_time_values_time_while_shorthanded | integer | Seconds the team played shorthanded in this game. |
| data_home_goal_keeper_changes | character | Goalie on/off events of that team. |
| data_away_goal_keeper_changes | character | Goalie on/off events of that team. |
| data_game_actions | character | Empty array in both samples; item shape unobserved. |
| data_faceoffs_head_to_head | character | Empty array in both samples; item shape unobserved. |
| data_faceoffs_individual | character | Empty array in both samples; item shape unobserved. |
| data_game_situations | character | Empty array in both samples; item shape unobserved. |
| calc_time | integer | Server computation time; `0` in the samples. |
| last_update_formatted_short | character | Short display text `DD.MM.YY` (plus `HH:MM` for times). |
| last_update_formatted_long | character | Long display text, e.g. `18.September. 2026`. |
| last_update_timestamp | numeric | Epoch milliseconds. |
| last_update_value | character | ISO text `YYYY-MM-DD` (plus ` HH:MM` for times). |
| served_by_server | character | Host that built the bundle, e.g. `icehl.hokejovyzapis.cz`. |

## Errors

Signalled as classed conditions: `fastRhockey_no_data` when the bucket
has no such key (S3 404 NoSuchKey: unknown season, competition, match or
team) or the file holds no rows (an unplayed game, an empty shot shell);
`fastRhockey_asset_fetch` for any other non-2xx answer (403
AccessDenied), a connection failure after retries, an empty or
unreadable 200 body, an error envelope, a body of the wrong shape, or a
body for another season or game; `fastRhockey_bad_request` for an HTTP
400/422 or an invalid argument. All three also inherit
`fastRhockey_error`. Wrappers raise; they never warn and return an empty
frame.

## See also

[Metal Ligaen](https://metalligaen.dk/) (the league site whose widgets
read this public eSports bucket)

Other Metal Ligaen eSports functions:
[`metal_game_info()`](https://fastRhockey.sportsdataverse.org/reference/metal_game_info.md),
[`metal_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_goalie_box.md),
[`metal_pbp()`](https://fastRhockey.sportsdataverse.org/reference/metal_pbp.md),
[`metal_player_box()`](https://fastRhockey.sportsdataverse.org/reference/metal_player_box.md),
[`metal_schedule()`](https://fastRhockey.sportsdataverse.org/reference/metal_schedule.md),
[`metal_standings()`](https://fastRhockey.sportsdataverse.org/reference/metal_standings.md),
[`metal_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/metal_top_scorers.md)

## Examples

``` r
# \donttest{
  try(metal_game_center(match_id = 575))
#> ── Metal Ligaen game_center from the eSports hokejovyzapis bucket ──────────────
#> ℹ Data updated: 2026-10-18 06:15:38 UTC
#> # A tibble: 1 × 125
#>   status_id status_msg data_game_data_id data_game_data_division_id
#>       <int> <chr>                  <int>                      <int>
#> 1         1 Ok                       575                          7
#> # ℹ 121 more variables: data_game_data_division_longname <chr>,
#> #   data_game_data_division_shortname <chr>,
#> #   data_game_data_scheduled_date_formatted_short <chr>,
#> #   data_game_data_scheduled_date_formatted_long <chr>,
#> #   data_game_data_scheduled_date_timestamp <dbl>,
#> #   data_game_data_scheduled_date_value <chr>,
#> #   data_game_data_scheduled_date_diff_to_now <int>, …
# }
```
