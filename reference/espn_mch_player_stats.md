# **Get ESPN MCH / WCH Player Stats (web-v3)**

**Get ESPN MCH / WCH Player Stats (web-v3)**

## Usage

``` r
espn_mch_player_stats(athlete_id, season = most_recent_nhl_season(), ...)

espn_wch_player_stats(athlete_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5136656"`).
  Obtain via
  [`espn_mch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_roster.md).

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).
  When `NULL` the current season is used.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per season entry plus a
career-total row per stat category:

|                      |           |                                            |
|----------------------|-----------|--------------------------------------------|
| col_name             | types     | description                                |
| athlete_id           | character | ESPN athlete identifier (echoed from arg). |
| season               | integer   | Requested season year (echoed from arg).   |
| season_year          | integer   | Season end-year for this row.              |
| category             | character | Stat category name (e.g. center).          |
| category_display     | character | Stat category display name.                |
| position             | character | Position for this season row.              |
| team_id              | character | ESPN team identifier.                      |
| team_slug            | character | Team slug.                                 |
| team_display_name    | character | Team display name.                         |
| team_abbreviation    | character | Team abbreviation.                         |
| games                | character | Games played.                              |
| goals                | character | Goals.                                     |
| assists              | character | Assists.                                   |
| points               | character | Points.                                    |
| plus_minus           | character | Plus/minus.                                |
| penalty_minutes      | character | Penalty minutes.                           |
| shootout_goals       | character | Shootout goals.                            |
| shooting_pct         | character | Shooting percentage.                       |
| power_play_goals     | character | Power-play goals.                          |
| power_play_assists   | character | Power-play assists.                        |
| short_handed_goals   | character | Short-handed goals.                        |
| short_handed_assists | character | Short-handed assists.                      |
| game_winning_goals   | character | Game-winning goals.                        |
| time_on_ice_per_game | character | Average time on ice per game.              |
| production           | character | Production metric.                         |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_stats_v3()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_stats_v3.md)
(columns inherited); a collection may be empty.

`espn_wch_player_stats()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_player_stats(athlete_id = "5136656", season = 2025))
#> ✖ 2026-11-13 21:26:26.604381: Invalid arguments or no ESPN mens-college-hockey player stats data available!
#> ✖ Args: league = "mens-college-hockey", athlete_id = "5136656", season = 2025
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_stats(athlete_id = "5263060", season = 2025))
#> ✖ 2026-11-13 21:26:26.641758: Invalid arguments or no ESPN womens-college-hockey player stats data available!
#> ✖ Args: league = "womens-college-hockey", athlete_id = "5263060", season = 2025
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
