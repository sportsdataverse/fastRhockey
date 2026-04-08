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

  The name of the team rosters data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_rosters(2022))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 1,192 × 9
#>    player_id full_name      first_name last_name team_abbr team_id position_code
#>        <int> <chr>          <chr>      <chr>     <chr>       <int> <chr>        
#>  1   8471709 Marc-Edouard … Marc-Edou… Vlasic    SJS            28 D            
#>  2   8473503 James Reimer   James      Reimer    SJS            28 G            
#>  3   8474009 Nick Bonino    Nick       Bonino    SJS            28 C            
#>  4   8474053 Logan Couture  Logan      Couture   SJS            28 C            
#>  5   8474151 Ryan McDonagh  Ryan       McDonagh  NSH            18 D            
#>  6   8474578 Erik Karlsson  Erik       Karlsson  SJS            28 D            
#>  7   8474600 Roman Josi     Roman      Josi      NSH            18 D            
#>  8   8474697 Mark Borowiec… Mark       Borowiec… NSH            18 D            
#>  9   8475168 Matt Duchene   Matt       Duchene   NSH            18 C            
#> 10   8475218 Mattias Ekholm Mattias    Ekholm    NSH            18 D            
#> # ℹ 1,182 more rows
#> # ℹ 2 more variables: sweater_number <int>, season <int>
# }
```
