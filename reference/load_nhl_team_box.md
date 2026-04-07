# **Load fastRhockey NHL team box scores**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_team_box(
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

  The name of the team box data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_team_box(2021))
#> ── NHL Team Boxscore Information from fastRhockey data repository ──────────────
#> ℹ Data updated: 2023-01-05 10:01:54 UTC
#> # A tibble: 1,904 × 18
#>    team_id team_name           link      abbreviation tri_code goals   pim shots
#>      <int> <chr>               <chr>     <chr>        <chr>    <int> <int> <int>
#>  1       8 Montréal Canadiens  /api/v1/… MTL          MTL          0     8    22
#>  2      14 Tampa Bay Lightning /api/v1/… TBL          TBL          1     8    30
#>  3      14 Tampa Bay Lightning /api/v1/… TBL          TBL          2    12    34
#>  4       8 Montréal Canadiens  /api/v1/… MTL          MTL          3    20    21
#>  5      14 Tampa Bay Lightning /api/v1/… TBL          TBL          6     2    30
#>  6       8 Montréal Canadiens  /api/v1/… MTL          MTL          3     2    35
#>  7       8 Montréal Canadiens  /api/v1/… MTL          MTL          1    20    43
#>  8      14 Tampa Bay Lightning /api/v1/… TBL          TBL          3    20    23
#>  9       8 Montréal Canadiens  /api/v1/… MTL          MTL          1     8    19
#> 10      14 Tampa Bay Lightning /api/v1/… TBL          TBL          5     6    27
#> # ℹ 1,894 more rows
#> # ℹ 10 more variables: power_play_percentage <chr>, power_play_goals <int>,
#> #   power_play_opportunities <int>, face_off_win_percentage <chr>,
#> #   blocked <int>, takeaways <int>, giveaways <int>, hits <int>, game_id <int>,
#> #   season <int>
# }
```
