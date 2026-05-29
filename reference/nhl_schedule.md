# **NHL Schedule**

Returns NHL schedule data for a given day or season. Uses the NHL API
(`api-web.nhle.com`).

## Usage

``` r
nhl_schedule(
  day = NULL,
  season = NULL,
  team_abbr = NULL,
  include_data_flags = FALSE,
  game_type = c("both", "regular", "playoffs")
)
```

## Arguments

- day:

  Character date in "YYYY-MM-DD" format. If provided, returns games for
  that specific day.

- season:

  Integer four-digit year for the *end year* of the season (e.g., 2026
  for the 2025-26 season), matching
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).
  If provided instead of `day`, returns the full season schedule.

- team_abbr:

  Character three-letter team abbreviation (e.g., "TOR"). Required when
  `season` is used. If NULL, loops through all teams.

- include_data_flags:

  Logical (default `FALSE`). When `TRUE`, after building the live
  schedule the result is left-joined against the pre-compiled
  `nhl_games_in_data_repo` index from
  [sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
  to add per-game data-availability flags (`PBP`, `team_box`,
  `player_box`, `skater_box`, `goalie_box`, `game_info`, `game_rosters`,
  `scoring`, `penalties`, `scratches`, `linescore`, `three_stars`,
  `shifts`, `officials`, `shots_by_period`, `shootout`). Games not yet
  compiled get `FALSE`. This requires a network call to the data repo
  and adds a small delay.

- game_type:

  Character, one of `"both"` (default), `"regular"`, or `"playoffs"`.
  Applies only in season mode; silently ignored when `day` is supplied.
  Default `"both"` returns regular-season and playoff games for the
  requested `season` in a single tibble.

## Value

A data frame (`fastRhockey_data`) with the following columns. When
`include_data_flags = TRUE` it additionally carries one logical column
per pre-compiled dataset.

|                |           |                                  |
|----------------|-----------|----------------------------------|
| col_name       | types     | description                      |
| game_id        | integer   | Unique game identifier.          |
| season_full    | character | Full 8-digit season identifier.  |
| game_type      | character | Game type code (PR/R/P/A).       |
| game_date      | character | Game date.                       |
| game_time      | character | Scheduled game start time (UTC). |
| home_team_abbr | character | Home team abbreviation.          |
| away_team_abbr | character | Away team abbreviation.          |
| home_team_name | character | Home team name.                  |
| away_team_name | character | Away team name.                  |
| home_score     | integer   | Home team final score.           |
| away_score     | integer   | Away team final score.           |
| game_state     | character | Current state of the game.       |
| venue          | character | Name of the venue.               |

## Examples

``` r
# \donttest{
  try(nhl_schedule(day = "2024-01-15"))
#> ── NHL Schedule ─────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:47:28 UTC
#> # A tibble: 53 × 13
#>       game_id season_full game_type game_date  game_time          home_team_abbr
#>         <int> <chr>       <chr>     <chr>      <chr>              <chr>         
#>  1 2023020672 20232024    R         2024-01-15 2024-01-15T17:00:… BUF           
#>  2 2023020671 20232024    R         2024-01-15 2024-01-15T18:00:… BOS           
#>  3 2023020673 20232024    R         2024-01-15 2024-01-15T18:00:… CBJ           
#>  4 2023020674 20232024    R         2024-01-15 2024-01-15T18:00:… FLA           
#>  5 2023020677 20232024    R         2024-01-15 2024-01-15T18:00:… PIT           
#>  6 2023020675 20232024    R         2024-01-15 2024-01-15T20:00:… CAR           
#>  7 2023020676 20232024    R         2024-01-15 2024-01-15T23:00:… MIN           
#>  8 2023020680 20232024    R         2024-01-15 2024-01-15T23:00:… VGK           
#>  9 2023020678 20232024    R         2024-01-15 2024-01-16T00:00:… MTL           
#> 10 2023020679 20232024    R         2024-01-15 2024-01-16T01:00:… STL           
#> # ℹ 43 more rows
#> # ℹ 7 more variables: away_team_abbr <chr>, home_team_name <chr>,
#> #   away_team_name <chr>, home_score <int>, away_score <int>, game_state <chr>,
#> #   venue <chr>
  try(nhl_schedule(season = 2025, team_abbr = "TOR"))
#> ── NHL Schedule ─────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:47:31 UTC
#> # A tibble: 95 × 16
#>       game_id season_full game_type game_date  game_time          home_team_abbr
#>         <int> <chr>       <chr>     <chr>      <chr>              <chr>         
#>  1 2024020006 20242025    R         2024-10-09 2024-10-09T23:00:… MTL           
#>  2 2024020015 20242025    R         2024-10-10 2024-10-10T23:00:… NJD           
#>  3 2024020026 20242025    R         2024-10-12 2024-10-12T23:00:… TOR           
#>  4 2024020058 20242025    R         2024-10-16 2024-10-16T23:30:… TOR           
#>  5 2024020079 20242025    R         2024-10-19 2024-10-19T23:00:… TOR           
#>  6 2024020091 20242025    R         2024-10-21 2024-10-21T23:30:… TOR           
#>  7 2024020097 20242025    R         2024-10-22 2024-10-22T23:30:… CBJ           
#>  8 2024020110 20242025    R         2024-10-24 2024-10-24T23:00:… TOR           
#>  9 2024020125 20242025    R         2024-10-26 2024-10-26T23:00:… BOS           
#> 10 2024020143 20242025    R         2024-10-28 2024-10-28T23:30:… WPG           
#> # ℹ 85 more rows
#> # ℹ 10 more variables: away_team_abbr <chr>, home_team_name <chr>,
#> #   away_team_name <chr>, home_score <int>, away_score <int>, game_state <chr>,
#> #   venue <chr>, series_letter <chr>, playoff_round <int>,
#> #   series_game_number <int>
  try(nhl_schedule(season = 2024, team_abbr = "TOR", game_type = "playoffs"))
#> ── NHL Schedule ─────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:47:35 UTC
#> # A tibble: 7 × 16
#>      game_id season_full game_type game_date  game_time           home_team_abbr
#>        <int> <chr>       <chr>     <chr>      <chr>               <chr>         
#> 1 2023030121 20232024    P         2024-04-21 2024-04-21T00:00:0… BOS           
#> 2 2023030122 20232024    P         2024-04-22 2024-04-22T23:00:0… BOS           
#> 3 2023030123 20232024    P         2024-04-24 2024-04-24T23:00:0… TOR           
#> 4 2023030124 20232024    P         2024-04-28 2024-04-28T00:00:0… TOR           
#> 5 2023030125 20232024    P         2024-04-30 2024-04-30T23:00:0… BOS           
#> 6 2023030126 20232024    P         2024-05-03 2024-05-03T00:00:0… TOR           
#> 7 2023030127 20232024    P         2024-05-05 2024-05-05T00:00:0… BOS           
#> # ℹ 10 more variables: away_team_abbr <chr>, home_team_name <chr>,
#> #   away_team_name <chr>, home_score <int>, away_score <int>, game_state <chr>,
#> #   venue <chr>, series_letter <chr>, playoff_round <int>,
#> #   series_game_number <int>
  try(nhl_schedule(day = "2024-01-15", include_data_flags = TRUE))
#> 2026-05-29 18:47:36.022187: Error fetching schedule for 2024-01-15: `x` and `y` must share the same src.
# }
```
