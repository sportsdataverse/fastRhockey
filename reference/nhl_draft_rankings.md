# **NHL Draft Rankings**

Returns NHL draft rankings for a given season and category.

## Usage

``` r
nhl_draft_rankings(season = NULL, category = 1)
```

## Arguments

- season:

  Integer 4-digit year (e.g., 2024). If NULL, returns current.

- category:

  Integer category: 1 = North American skaters, 2 = International
  skaters, 3 = North American goalies, 4 = International goalies.
  Default 1.

## Value

Returns a data frame with draft ranking data.

## Examples

``` r
# \donttest{
  try(nhl_draft_rankings())
#> ── NHL Draft Rankings ───────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 07:09:16 UTC
#> # A tibble: 225 × 13
#>    last_name first_name position_code shoots_catches height_in_inches
#>    <chr>     <chr>      <chr>         <chr>                     <int>
#>  1 McKenna   Gavin      LW            L                            71
#>  2 Verhoeff  Keaton     D             R                            76
#>  3 Carels    Carson     D             L                            74
#>  4 Reid      Chase      D             R                            74
#>  5 Malhotra  Caleb      C             L                            74
#>  6 Rudolph   Daxon      D             R                            74
#>  7 Lawrence  Tynan      C             L                            72
#>  8 Morozov   Ilya       C             L                            75
#>  9 Belchetz  Ethan      LW            L                            77
#> 10 Hurlbert  JP         LW            R                            72
#> # ℹ 215 more rows
#> # ℹ 8 more variables: weight_in_pounds <int>, last_amateur_club <chr>,
#> #   last_amateur_league <chr>, birth_date <chr>, birth_city <chr>,
#> #   birth_state_province <chr>, birth_country <chr>, midterm_rank <int>
# }
```
