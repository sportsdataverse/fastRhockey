# **NHL Draft Prospects**

Returns current draft prospect rankings.

Uses the new NHL API endpoint at
`api-web.nhle.com/v1/draft/rankings/now`.

## Usage

``` r
nhl_draft_prospects()
```

## Value

Returns a data frame of draft prospect rankings.

## Examples

``` r
# \donttest{
   try(nhl_draft_prospects())
#> ── NHL Draft Prospects data from NHL.com ────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-07 13:28:30 UTC
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
