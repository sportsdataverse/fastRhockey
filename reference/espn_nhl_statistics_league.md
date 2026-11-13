# **Get ESPN NHL League Statistics**

**Get ESPN NHL League Statistics**

## Usage

``` r
espn_nhl_statistics_league(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (stat category, leader).
The ESPN NHL statistics endpoint returns a sparse set of top-of-league
stat leaders organized by category. If the endpoint returns no
categories, an empty tibble is returned gracefully:

|  |  |  |
|----|----|----|
| col_name | types | description |
| category_name | character | Stat category name (e.g. "goals", "assists"). |
| category_abbreviation | character | Category abbreviation. |
| display_value | character | Leader stat display value. |
| value | numeric | Leader stat numeric value. |
| athlete_id | character | ESPN athlete identifier. |
| athlete_display_name | character | Athlete display name. |
| team_id | character | ESPN team identifier. |
| team_abbreviation | character | Team abbreviation. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_statistics_league())
#> ── NHL League Statistics data from ESPN.com ─────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:49 UTC
#> # A tibble: 394 × 8
#>    category_name category_abbreviation display_value value athlete_id
#>    <chr>         <chr>                 <chr>         <dbl> <chr>     
#>  1 goals         G                     4                 4 5216858   
#>  2 goals         G                     3                 3 5291934   
#>  3 goals         G                     3                 3 4319858   
#>  4 goals         G                     3                 3 5136616   
#>  5 goals         G                     3                 3 5361709   
#>  6 goals         G                     3                 3 2562602   
#>  7 goals         G                     3                 3 4272192   
#>  8 goals         G                     3                 3 4894702   
#>  9 goals         G                     3                 3 4565225   
#> 10 goals         G                     3                 3 5188613   
#> # ℹ 384 more rows
#> # ℹ 3 more variables: athlete_display_name <chr>, team_id <chr>,
#> #   team_abbreviation <chr>
# }
```
