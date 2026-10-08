# **Load NHL team conference and division memberships by season from the SportsDataverse data repo**

One row per NHL team per season, with the conference and division it
played in that season, e.g. the Detroit Red Wings moving to
`nhl:atlantic` in the 2014 (2013-14) realignment. Published to the
`nhl_groups` release tag on the sportsdataverse-data releases, one file
per season.

## Usage

``` r
load_nhl_team_group_seasons(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season), or `TRUE` for every published season.
  Min: 1918. There is no 2005 (the 2004-05 lockout).

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
| season | integer | Season (end year). |
| team_id | character | ESPN team id where ESPN covers the team, otherwise the NHL team id. |
| team_id_source | character | Id system of `team_id`: `espn` or `nhl`. |
| team_name | character | Team name as of that season. |
| subdivision_id | character | SportsDataverse subdivision group id; `NA` for the NHL. |
| conference_id | character | SportsDataverse conference group id; `NA` where the level does not apply. |
| division_id | character | SportsDataverse division group id; `NA` where the level does not apply. |
| source | character | Source the membership came from. |
| sources_agree | logical | Whether a second source agrees; `NA` when only one source covers the season. |
| notes | character | Notes, e.g. the NHL's own team id. |

## Examples

``` r
# \donttest{
  try(load_nhl_team_group_seasons(seasons = 2014))
#> ── NHL team group seasons from the SportsDataverse data repo ───────────────────
#> ℹ Data updated: 2026-10-08 12:58:29 UTC
#> # A tibble: 30 × 11
#>    league season team_id team_id_source team_name   subdivision_id conference_id
#>    <chr>   <int> <chr>   <chr>          <chr>       <chr>          <chr>        
#>  1 nhl      2014 1       espn           Boston Bru… NA             nhl:eastern  
#>  2 nhl      2014 10      espn           Montréal C… NA             nhl:eastern  
#>  3 nhl      2014 11      espn           New Jersey… NA             nhl:eastern  
#>  4 nhl      2014 12      espn           New York I… NA             nhl:eastern  
#>  5 nhl      2014 13      espn           New York R… NA             nhl:eastern  
#>  6 nhl      2014 14      espn           Ottawa Sen… NA             nhl:eastern  
#>  7 nhl      2014 15      espn           Philadelph… NA             nhl:eastern  
#>  8 nhl      2014 16      espn           Pittsburgh… NA             nhl:eastern  
#>  9 nhl      2014 17      espn           Colorado A… NA             nhl:western  
#> 10 nhl      2014 18      espn           San Jose S… NA             nhl:western  
#> # ℹ 20 more rows
#> # ℹ 4 more variables: division_id <chr>, source <chr>, sources_agree <lgl>,
#> #   notes <chr>
# }
```
