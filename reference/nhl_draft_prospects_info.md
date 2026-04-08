# **NHL Draft Prospects Info**

Returns draft prospect rankings for a given year and prospect category.

Uses the new NHL API endpoint at
`api-web.nhle.com/v1/draft/rankings/{year}/{prospect_category}`.

The original per-prospect-ID endpoint is no longer available. This
function now returns rankings filtered by year and category.

## Usage

``` r
nhl_draft_prospects_info(year, prospect_category = 1)
```

## Arguments

- year:

  Integer. Draft year (e.g. 2024).

- prospect_category:

  Integer. Prospect category:

  - 1 = North American Skater

  - 2 = International Skater

  - 3 = North American Goalie

  - 4 = International Goalie

## Value

Returns a data frame of draft prospect rankings.

## Examples

``` r
# \donttest{
   try(nhl_draft_prospects_info(year = 2024, prospect_category = 1))
#> ── NHL Draft Prospects Information from NHL.com ─────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:16:40 UTC
#> # A tibble: 251 × 14
#>    last_name first_name position_code shoots_catches height_in_inches
#>    <chr>     <chr>      <chr>         <chr>                     <int>
#>  1 Celebrini Macklin    C             L                            72
#>  2 Levshunov Artyom     D             R                            74
#>  3 Lindstrom Cayden     C             L                            75
#>  4 Buium     Zeev       D             L                            72
#>  5 Parekh    Zayne      D             R                            72
#>  6 Connelly  Trevor     LW            L                            73
#>  7 Dickinson Sam        D             L                            75
#>  8 Catton    Berkly     C             L                            70
#>  9 Iginla    Tij        C             L                            72
#> 10 Hage      Michael    C             R                            73
#> # ℹ 241 more rows
#> # ℹ 9 more variables: weight_in_pounds <int>, last_amateur_club <chr>,
#> #   last_amateur_league <chr>, birth_date <chr>, birth_city <chr>,
#> #   birth_state_province <chr>, birth_country <chr>, midterm_rank <int>,
#> #   final_rank <int>
# }
```
