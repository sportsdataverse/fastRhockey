# **Load fastRhockey NHL team rosters**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_rosters(
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
  [`update_phf_db()`](https://fastRhockey.sportsdataverse.org/reference/update_phf_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the team rosters data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_rosters(2021))
#> ── NHL Team Roster Information from fastRhockey data repository ────────────────
#> ℹ Data updated: 2023-01-05 10:02:24 UTC
#> # A tibble: 1,078 × 10
#>    jersey_number player_id player_full_name player_link            position_code
#>    <chr>             <int> <chr>            <chr>                  <chr>        
#>  1 14              8471233 Travis Zajac     /api/v1/people/8471233 C            
#>  2 76              8474056 P.K. Subban      /api/v1/people/8474056 D            
#>  3 21              8475151 Kyle Palmieri    /api/v1/people/8475151 C            
#>  4 29              8475179 Dmitry Kulikov   /api/v1/people/8475179 D            
#>  5 45              8475222 Sami Vatanen     /api/v1/people/8475222 D            
#>  6 7               8476807 Matt Tennyson    /api/v1/people/8476807 D            
#>  7 28              8476850 Ryan Murray      /api/v1/people/8476850 D            
#>  8 28              8476923 Damon Severson   /api/v1/people/8476923 D            
#>  9 58              8476941 Connor Carrick   /api/v1/people/8476941 D            
#> 10 97              8477038 Nikita Gusev     /api/v1/people/8477038 L            
#> # ℹ 1,068 more rows
#> # ℹ 5 more variables: position_name <chr>, position_type <chr>,
#> #   position_abbreviation <chr>, team_id <int>, season <int>
# }
```
