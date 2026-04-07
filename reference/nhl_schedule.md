# **NHL Schedule**

Returns NHL schedule data for a given day or season. Uses the NHL API
(`api-web.nhle.com`).

## Usage

``` r
nhl_schedule(day = NULL, season = NULL, team_abbr = NULL)
```

## Arguments

- day:

  Character date in "YYYY-MM-DD" format. If provided, returns games for
  that specific day.

- season:

  Integer four-digit year for the start of the season (e.g., 2024 for
  the 2024-25 season). If provided instead of `day`, returns the full
  season schedule.

- team_abbr:

  Character three-letter team abbreviation (e.g., "TOR"). Required when
  `season` is used. If NULL, loops through all teams.

## Value

Returns a data frame with game schedule information.

## Examples

``` r
# \donttest{
  try(nhl_schedule(day = "2024-01-15"))
#> ── NHL Schedule ─────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:15:44 UTC
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
  try(nhl_schedule(season = 2024, team_abbr = "TOR"))
#> ── NHL Schedule ─────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:15:45 UTC
#> # A tibble: 101 × 13
#>       game_id season_full game_type game_date  game_time          home_team_abbr
#>         <int> <chr>       <chr>     <chr>      <chr>              <chr>         
#>  1 2024010010 20242025    PR        2024-09-22 2024-09-22T23:00:… TOR           
#>  2 2024010024 20242025    PR        2024-09-24 2024-09-24T23:00:… OTT           
#>  3 2024010038 20242025    PR        2024-09-26 2024-09-26T23:00:… TOR           
#>  4 2024010055 20242025    PR        2024-09-28 2024-09-28T23:00:… MTL           
#>  5 2024010086 20242025    PR        2024-10-03 2024-10-03T23:00:… DET           
#>  6 2024010100 20242025    PR        2024-10-05 2024-10-05T23:00:… TOR           
#>  7 2024020006 20242025    R         2024-10-09 2024-10-09T23:00:… MTL           
#>  8 2024020015 20242025    R         2024-10-10 2024-10-10T23:00:… NJD           
#>  9 2024020026 20242025    R         2024-10-12 2024-10-12T23:00:… TOR           
#> 10 2024020058 20242025    R         2024-10-16 2024-10-16T23:30:… TOR           
#> # ℹ 91 more rows
#> # ℹ 7 more variables: away_team_abbr <chr>, home_team_name <chr>,
#> #   away_team_name <chr>, home_score <int>, away_score <int>, game_state <chr>,
#> #   venue <chr>
# }
```
