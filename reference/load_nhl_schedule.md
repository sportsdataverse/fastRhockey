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

  The name of the schedule data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_schedule(2021))
#> ── NHL Schedule Information from fastRhockey data repository ───────────────────
#> ℹ Data updated: 2023-01-05 10:02:15 UTC
#> # A tibble: 952 × 28
#>       game_id link        game_type_abbreviation season_full game_date_time     
#>         <int> <chr>       <chr>                        <int> <dttm>             
#>  1 2020030415 /api/v1/ga… P                         20202021 2021-07-08 00:00:00
#>  2 2020030414 /api/v1/ga… P                         20202021 2021-07-06 00:00:00
#>  3 2020030413 /api/v1/ga… P                         20202021 2021-07-03 00:00:00
#>  4 2020030412 /api/v1/ga… P                         20202021 2021-07-01 00:00:00
#>  5 2020030411 /api/v1/ga… P                         20202021 2021-06-29 00:00:00
#>  6 2020030327 /api/v1/ga… P                         20202021 2021-06-26 00:00:00
#>  7 2020030316 /api/v1/ga… P                         20202021 2021-06-25 00:00:00
#>  8 2020030326 /api/v1/ga… P                         20202021 2021-06-24 00:00:00
#>  9 2020030315 /api/v1/ga… P                         20202021 2021-06-23 01:00:00
#> 10 2020030325 /api/v1/ga… P                         20202021 2021-06-22 00:00:00
#> # ℹ 942 more rows
#> # ℹ 23 more variables: status_abstract_game_state <chr>,
#> #   status_coded_game_state <int>, status_detailed_state <chr>,
#> #   status_status_code <int>, status_start_time_tbd <lgl>, away_score <int>,
#> #   away_team_id <int>, away_team_name <chr>, away_team_link <chr>,
#> #   home_score <int>, home_team_id <int>, home_team_name <chr>,
#> #   home_team_link <chr>, venue_name <chr>, venue_link <chr>, venue_id <int>, …
# }
```
