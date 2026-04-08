# **Load fastRhockey NHL schedules**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_schedule(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2021.

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the schedule data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_schedule(2022))
#> ── NHL Schedule from fastRhockey data repository ────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 05:45:20 UTC
#> # A tibble: 1,504 × 19
#>       game_id season_full game_type game_date  game_time          home_team_abbr
#>         <int> <chr>       <chr>     <chr>      <chr>              <chr>         
#>  1 2021030416 20212022    P         2022-06-26 2022-06-27T00:00:… TBL           
#>  2 2021030415 20212022    P         2022-06-24 2022-06-25T00:00:… COL           
#>  3 2021030414 20212022    P         2022-06-22 2022-06-23T00:00:… TBL           
#>  4 2021030413 20212022    P         2022-06-20 2022-06-21T00:00:… TBL           
#>  5 2021030412 20212022    P         2022-06-18 2022-06-19T00:00:… COL           
#>  6 2021030411 20212022    P         2022-06-15 2022-06-16T00:00:… COL           
#>  7 2021030316 20212022    P         2022-06-11 2022-06-12T00:00:… TBL           
#>  8 2021030315 20212022    P         2022-06-09 2022-06-10T00:00:… NYR           
#>  9 2021030314 20212022    P         2022-06-07 2022-06-08T00:00:… TBL           
#> 10 2021030324 20212022    P         2022-06-06 2022-06-07T00:00:… EDM           
#> # ℹ 1,494 more rows
#> # ℹ 13 more variables: away_team_abbr <chr>, home_team_name <chr>,
#> #   away_team_name <chr>, home_score <int>, away_score <int>, game_state <chr>,
#> #   venue <chr>, season <int>, game_json <lgl>, game_json_url <glue>,
#> #   PBP <lgl>, team_box <lgl>, player_box <lgl>
# }
```
