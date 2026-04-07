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

  A vector of 4-digit years associated with given NHL seasons. (Min:
  2011)

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
  try(load_nhl_pbp(2021))
#> ── NHL Play-by-Play Information from fastRhockey data repository ───────────────
#> ℹ Data updated: 2023-01-05 10:01:13 UTC
#> # A tibble: 593,081 × 105
#>    event_type     event description period period_seconds period_seconds_remai…¹
#>    <chr>          <chr> <chr>        <int>          <int>                  <int>
#>  1 GAME_SCHEDULED Game… Game Sched…      1              0                   1200
#>  2 CHANGE         Chan… ON: Justin…      1              0                   1200
#>  3 CHANGE         Chan… ON: Sidney…      1              0                   1200
#>  4 FACEOFF        Face… Sidney Cro…      1              0                   1200
#>  5 SHOT           Shot  Travis Kon…      1             16                   1184
#>  6 TAKEAWAY       Take… Takeaway b…      1             23                   1177
#>  7 BLOCKED_SHOT   Bloc… Sean Coutu…      1             28                   1172
#>  8 SHOT           Shot  Evan Rodri…      1             34                   1166
#>  9 CHANGE         Chan… ON: Claude…      1             34                   1166
#> 10 CHANGE         Chan… ON: Evgeni…      1             35                   1165
#> # ℹ 593,071 more rows
#> # ℹ abbreviated name: ¹​period_seconds_remaining
#> # ℹ 99 more variables: game_seconds <int>, game_seconds_remaining <int>,
#> #   home_score <int>, away_score <int>, strength_state <chr>, event_idx <chr>,
#> #   extra_attacker <lgl>, home_skaters <int>, away_skaters <int>,
#> #   game_id <int>, period_type <chr>, ordinal_num <chr>, period_time <chr>,
#> #   period_time_remaining <chr>, date_time <chr>, home_final <int>, …
# }
```
