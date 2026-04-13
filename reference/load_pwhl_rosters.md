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
#> # A tibble: 174 × 16
#>    player_id team_id season player_name  first_name last_name primary_hand dob  
#>        <dbl>   <dbl>  <int> <chr>        <chr>      <chr>     <chr>        <chr>
#>  1        49       1   2024 Lexie Adzija Lexie      Adzija    L            2000…
#>  2        18       1   2024 Sophie Shir… Sophie     Shirley   R            1999…
#>  3        15       1   2024 Alina Müller Alina      Müller    L            1998…
#>  4       111       1   2024 Taylor Wenc… Taylor     Wenczkow… R            1997…
#>  5       176       1   2024 Caitrin Lon… Caitrin    Lonergan  R            1997…
#>  6       110       1   2024 Amanda Pelk… Amanda     Pelkey    R            1993…
#>  7         9       1   2024 Taylor Gira… Taylor     Girard    L            1998…
#>  8       109       1   2024 Gigi Marvin  Gigi       Marvin    R            1987…
#>  9         1       1   2024 Hannah Bran… Hannah     Brandt    R            1993…
#> 10        13       1   2024 Hilary Knig… Hilary     Knight    R            1989…
#> # ℹ 164 more rows
#> # ℹ 8 more variables: height <lgl>, position <chr>, home_town <chr>,
#> #   league <chr>, age <dbl>, player_headshot <chr>, regular_season <lgl>,
#> #   team <chr>
# }
```
