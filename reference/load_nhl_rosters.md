# **Load fastRhockey NHL team rosters**

Helper that loads multiple seasons of pre-scraped NHL roster data
(unique players per season, de-duplicated from per-game rosters) from
the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases.

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

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the team rosters data table within the database

## Value

A data frame of class `fastRhockey_data`

## Examples

``` r
# \donttest{
  try(load_nhl_rosters(2022))
#> ── NHL Rosters from fastRhockey data repository ─────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:02:31 UTC
#> # A tibble: 1,295 × 9
#>    player_id full_name      first_name last_name team_abbr team_id position_code
#>        <int> <chr>          <chr>      <chr>     <chr>       <int> <chr>        
#>  1   8470604 Jeff Carter    Jeff       Carter    PIT             5 C            
#>  2   8470619 Brian Boyle    Brian      Boyle     PIT             5 C            
#>  3   8470621 Corey Perry    Corey      Perry     TBL            14 R            
#>  4   8470880 Brian Elliott  Brian      Elliott   TBL            14 G            
#>  5   8471724 Kris Letang    Kris       Letang    PIT             5 D            
#>  6   8473986 Alex Killorn   Alex       Killorn   TBL            14 C            
#>  7   8474034 Patrick Maroon Patrick    Maroon    TBL            14 L            
#>  8   8474151 Ryan McDonagh  Ryan       McDonagh  TBL            14 D            
#>  9   8474564 Steven Stamkos Steven     Stamkos   TBL            14 C            
#> 10   8474567 Zach Bogosian  Zach       Bogosian  TBL            14 D            
#> # ℹ 1,285 more rows
#> # ℹ 2 more variables: sweater_number <int>, season <int>
# }
```
