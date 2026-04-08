# **Load fastRhockey NHL play-by-play**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_pbp(
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

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the play by play data table within the database

## Value

A dataframe

## Examples

``` r
# \donttest{
  try(load_nhl_pbp(2022))
#> ── NHL Play-by-Play (full, with shifts) from fastRhockey data repository ───────
#> ℹ Data updated: 2026-04-08 05:53:06 UTC
#> # A tibble: 975,777 × 92
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <chr>      
#>  1 CHANGE       Chan… NA             NA              NA              ON: Jeff C…
#>  2 CHANGE       Chan… NA             NA              NA              ON: Ryan M…
#>  3 FACEOFF      Face… NA             PIT             away            Jeff Carte…
#>  4 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  5 HIT          Hit   NA             TBL             home            Ondrej Pal…
#>  6 CHANGE       Chan… NA             NA              NA              ON: Marcus…
#>  7 CHANGE       Chan… NA             NA              NA              ON: Alex K…
#>  8 CHANGE       Chan… NA             NA              NA              ON: Jason …
#>  9 STOP         Stop… NA             NA              NA              Stoppage i…
#> 10 FACEOFF      Face… NA             TBL             home            Anthony Ci…
#> # ℹ 975,767 more rows
#> # ℹ 86 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <int>, period_seconds_remaining <int>,
#> #   period_time_remaining <chr>, game_seconds <int>,
#> #   game_seconds_remaining <int>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
# }
```
