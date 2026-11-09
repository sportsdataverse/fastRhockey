# **Get ESPN NHL Player Stats (web-v3)**

**Get ESPN NHL Player Stats (web-v3)**

## Usage

``` r
espn_nhl_player_stats_v3(athlete_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5149125"`).
  Obtain via
  [`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).

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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_stats_v3(athlete_id = "5149125", season = 2025))
#> ── NHL Player Stats V3 data from ESPN.com ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:01 UTC
#> # A tibble: 5 × 25
#>   athlete_id season season_year category category_display  position team_id
#>   <chr>       <int>       <int> <chr>    <chr>             <chr>    <chr>  
#> 1 5149125      2025        2024 center   "Regular Season " C        4      
#> 2 5149125      2025        2025 center   "Regular Season " C        4      
#> 3 5149125      2025        2026 center   "Regular Season " C        4      
#> 4 5149125      2025        2027 center   "Regular Season " C        4      
#> 5 5149125      2025          NA center   "Regular Season " NA       TOTAL  
#> # ℹ 18 more variables: team_slug <chr>, team_display_name <chr>,
#> #   team_abbreviation <chr>, games <chr>, goals <chr>, assists <chr>,
#> #   points <chr>, plus_minus <chr>, penalty_minutes <chr>,
#> #   shootout_goals <chr>, shooting_pct <chr>, power_play_goals <chr>,
#> #   power_play_assists <chr>, short_handed_goals <chr>,
#> #   short_handed_assists <chr>, game_winning_goals <chr>,
#> #   time_on_ice_per_game <chr>, production <chr>
# }
```
