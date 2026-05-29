# **Load fastRhockey PWHL team rosters**

Helper that loads multiple seasons of pre-scraped PWHL roster data from
the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_pwhl_rosters(
  seasons = most_recent_pwhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the rosters data table within the database

## Value

A data frame of class `fastRhockey_data`

## Examples

``` r
# \donttest{
  try(load_pwhl_rosters(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 604 × 12
#>    team_id team   team_abbr team_side player_type player_id first_name last_name
#>      <int> <chr>  <chr>     <chr>     <chr>           <int> <chr>      <chr>    
#>  1       6 PWHL … TOR       home      skater             71 Jocelyne   Larocque 
#>  2       6 PWHL … TOR       home      skater            133 Lauriane   Rougeau  
#>  3       6 PWHL … TOR       home      skater             68 Kali       Flanagan 
#>  4       6 PWHL … TOR       home      skater            131 Olivia     Knowles  
#>  5       6 PWHL … TOR       home      skater            101 Alexa      Vasko    
#>  6       6 PWHL … TOR       home      skater             74 Allie      Munroe   
#>  7       6 PWHL … TOR       home      skater             67 Renata     Fast     
#>  8       6 PWHL … TOR       home      skater            126 Samantha   Cogan    
#>  9       6 PWHL … TOR       home      skater             65 Jesse      Compher  
#> 10       6 PWHL … TOR       home      skater             72 Rebecca    Leslie   
#> # ℹ 594 more rows
#> # ℹ 4 more variables: jersey_number <int>, position <chr>, birth_date <chr>,
#> #   season <int>
# }
```
