# **Load cleaned NHL play-by-play (lite) from the data repo**

Same as
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
but without line change (CHANGE) events, resulting in smaller file
sizes.

## Usage

``` r
load_nhl_pbp_lite(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A dataframe

## Examples

``` r
# \donttest{
  try(load_nhl_pbp_lite(2026))
#> ── NHL Play-by-Play (lite, no CHANGE events) from fastRhockey data repository ──
#> ℹ Data updated: 2026-04-08 06:01:19 UTC
#> # A tibble: 389,587 × 93
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <chr>      
#>  1 FACEOFF      Face… NA             CHI             away            Jason Dick…
#>  2 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  3 BLOCKED_SHOT Bloc… NA             CHI             away            Brad March…
#>  4 STOP         Stop… NA             NA              NA              Stoppage i…
#>  5 FACEOFF      Face… NA             CHI             away            Jason Dick…
#>  6 GIVEAWAY     Give… NA             FLA             home            Giveaway b…
#>  7 GIVEAWAY     Give… NA             CHI             away            Giveaway b…
#>  8 BLOCKED_SHOT Bloc… NA             CHI             away            Mackie Sam…
#>  9 STOP         Stop… NA             NA              NA              Stoppage i…
#> 10 FACEOFF      Face… NA             FLA             home            Jesper Boq…
#> # ℹ 389,577 more rows
#> # ℹ 87 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <int>, period_seconds_remaining <int>,
#> #   period_time_remaining <chr>, game_seconds <int>,
#> #   game_seconds_remaining <int>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
# }
```
