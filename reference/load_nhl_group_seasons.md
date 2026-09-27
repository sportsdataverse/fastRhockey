# **Load NHL conference and division names and parents by season from the SportsDataverse data repo**

One row per NHL group per season it existed, with the name, abbreviation
and parent group **as of that season** (not today's). Published to the
`nhl_groups` release tag on the sportsdataverse-data releases. Seasons
are keyed by their *end year* (2025 = 2024-25).

## Usage

``` r
load_nhl_group_seasons(..., dbConnection = NULL, tablename = NULL)
```

## Arguments

- ...:

  Additional arguments passed to an underlying function that writes the
  data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| league | character | League key. |
| group_id | character | SportsDataverse group id, `{league}:{slug}`. |
| season | integer | Season (end year). |
| level | character | Group level: `league`, `conference` or `division`. |
| name | character | Group name as of that season. |
| short_name | character | Short name as of that season. |
| abbreviation | character | Abbreviation as of that season. |
| parent_group_id | character | Parent group id as of that season (division, conference, league). |
| n_teams | integer | Member teams that season. |

## Examples

``` r
# \donttest{
  try(load_nhl_group_seasons())
#> ── NHL group seasons from the SportsDataverse data repo ─── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-27 04:35:52 UTC
#> # A tibble: 478 × 9
#>    league group_id    season level name  short_name abbreviation parent_group_id
#>    <chr>  <chr>        <int> <chr> <chr> <chr>      <chr>        <chr>          
#>  1 nhl    nhl:adams-…   1975 divi… Adam… Adams      ADM          nhl:eastern    
#>  2 nhl    nhl:adams-…   1976 divi… Adam… Adams      ADM          nhl:eastern    
#>  3 nhl    nhl:adams-…   1977 divi… Adam… Adams      ADM          nhl:eastern    
#>  4 nhl    nhl:adams-…   1978 divi… Adam… Adams      ADM          nhl:eastern    
#>  5 nhl    nhl:adams-…   1979 divi… Adam… Adams      ADM          nhl:eastern    
#>  6 nhl    nhl:adams-…   1980 divi… Adam… Adams      ADM          nhl:eastern    
#>  7 nhl    nhl:adams-…   1981 divi… Adam… Adams      ADM          nhl:eastern    
#>  8 nhl    nhl:adams-…   1982 divi… Adam… Adams      ADM          nhl:eastern    
#>  9 nhl    nhl:adams-…   1983 divi… Adam… Adams      ADM          nhl:eastern    
#> 10 nhl    nhl:adams-…   1984 divi… Adam… Adams      ADM          nhl:eastern    
#> # ℹ 468 more rows
#> # ℹ 1 more variable: n_teams <int>
# }
```
