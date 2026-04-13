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

A data frame of class `fastRhockey_data`. Common columns include:

|                    |           |                               |
|--------------------|-----------|-------------------------------|
| column             | type      | description                   |
| `game_id`          | integer   | PWHL game id                  |
| `team_id`          | integer   | HockeyTech team id            |
| `team`             | character | team full name                |
| `team_abbr`        | character | three-letter abbreviation     |
| `team_side`        | character | `"home"` or `"away"`          |
| `goals`            | integer   | goals scored                  |
| `shots`            | integer   | shots on goal                 |
| `pp_goals`         | integer   | power-play goals              |
| `pp_opportunities` | integer   | power-play opportunities      |
| `penalty_minutes`  | integer   | total PIM                     |
| `infraction_count` | integer   | number of infractions         |
| `faceoff_attempts` | integer   | faceoff attempts              |
| `faceoff_wins`     | integer   | faceoff wins                  |
| `faceoff_win_pct`  | numeric   | faceoff win percentage        |
| `season_record`    | character | season record after this game |

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
