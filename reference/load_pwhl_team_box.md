# **Load fastRhockey PWHL team box scores**

Helper that loads multiple seasons of pre-scraped PWHL team-level box
score data from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_team_boxscores`. Two rows per game (one per
side). File naming convention: `team_box_{end_year}.rds`.

## Usage

``` r
load_pwhl_team_box(
  seasons = most_recent_pwhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments (currently unused; kept for API symmetry).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the team box data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                  |           |                                        |
|------------------|-----------|----------------------------------------|
| col_name         | types     | description                            |
| game_id          | integer   | Unique game identifier.                |
| team_id          | integer   | Unique team identifier.                |
| team             | character | Team name.                             |
| team_abbr        | character | Team abbreviation.                     |
| team_side        | character | Home or away indicator.                |
| shots            | integer   | Shots on goal.                         |
| goals            | integer   | Goals scored.                          |
| hits             | integer   | Hits.                                  |
| pp_goals         | integer   | Power-play goals.                      |
| pp_opportunities | integer   | Power-play opportunities.              |
| goal_count       | integer   | Total goals recorded.                  |
| assist_count     | integer   | Total assists recorded.                |
| penalty_minutes  | integer   | Penalty minutes.                       |
| infraction_count | integer   | Number of infractions.                 |
| faceoff_attempts | integer   | Faceoff attempts.                      |
| faceoff_wins     | integer   | Faceoff wins.                          |
| faceoff_win_pct  | numeric   | Faceoff win percentage.                |
| season_wins      | integer   | Season wins entering/after the game.   |
| season_losses    | integer   | Season losses entering/after the game. |
| season_ot_wins   | integer   | Season overtime wins.                  |
| season_ot_losses | integer   | Season overtime losses.                |
| season_so_losses | integer   | Season shootout losses.                |
| season_record    | character | Season record after this game.         |

## Examples

``` r
# \donttest{
  try(load_pwhl_team_box(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 170 × 23
#>    game_id team_id team           team_abbr team_side shots goals  hits pp_goals
#>      <int>   <int> <chr>          <chr>     <chr>     <int> <int> <int>    <int>
#>  1       2       6 PWHL Toronto   TOR       home         29     0     0        0
#>  2       2       4 PWHL New York  NY        away         28     4     0        0
#>  3       3       5 PWHL Ottawa    OTT       home         28     2     0        1
#>  4       3       3 PWHL Montreal  MTL       away         24     3     0        0
#>  5       4       1 PWHL Boston    BOS       home         35     2     0        1
#>  6       4       2 PWHL Minnesota MIN       away         16     3     0        0
#>  7       5       4 PWHL New York  NY        home         31     2     0        0
#>  8       5       6 PWHL Toronto   TOR       away         37     3     0        1
#>  9       6       2 PWHL Minnesota MIN       home         22     3     0        0
#> 10       6       3 PWHL Montreal  MTL       away         24     0     0        0
#> # ℹ 160 more rows
#> # ℹ 14 more variables: pp_opportunities <int>, goal_count <int>,
#> #   assist_count <int>, penalty_minutes <int>, infraction_count <int>,
#> #   faceoff_attempts <int>, faceoff_wins <int>, faceoff_win_pct <dbl>,
#> #   season_wins <int>, season_losses <int>, season_ot_wins <int>,
#> #   season_ot_losses <int>, season_so_losses <int>, season_record <chr>
# }
```
