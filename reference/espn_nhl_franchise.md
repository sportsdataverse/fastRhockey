# **Get ESPN NHL Franchise (core-v2)**

**Get ESPN NHL Franchise (core-v2)**

## Usage

``` r
espn_nhl_franchise(franchise_id, ...)
```

## Arguments

- franchise_id:

  ESPN franchise identifier (character or numeric). Use
  [`espn_nhl_franchises()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_franchises.md)
  to discover valid franchise ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with franchise detail:

|  |  |  |
|----|----|----|
| col_name | types | description |
| franchise_id | character | ESPN franchise id (echoed from arg). |
| id | character | ESPN franchise identifier from response. |
| uid | character | Franchise uid string. |
| slug | character | Franchise URL slug. |
| location | character | Franchise city/location. |
| name | character | Franchise mascot name. |
| nickname | character | Franchise nickname. |
| abbreviation | character | Franchise abbreviation. |
| display_name | character | Franchise display name. |
| short_display_name | character | Franchise short display name. |
| color | character | Franchise primary color hex. |
| is_active | logical | Whether the franchise is currently active. |
| venue_ref | character | `$ref` URL for the franchise's current venue. |
| team_ref | character | `$ref` URL for the current season team object. |
| franchise_ref | character | `$ref` URL for this franchise object. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    fcs <- espn_nhl_franchises()
    espn_nhl_franchise(franchise_id = fcs$franchise_id[1])
  })
#> ── NHL Franchise data from ESPN core-v2 ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:16:26 UTC
#> # A tibble: 1 × 15
#>   franchise_id id    uid           slug     location name  nickname abbreviation
#>   <chr>        <chr> <chr>         <chr>    <chr>    <chr> <chr>    <chr>       
#> 1 1            1     s:70~l:90~f:1 boston-… Boston   Brui… Bruins   BOS         
#> # ℹ 7 more variables: display_name <chr>, short_display_name <chr>,
#> #   color <chr>, is_active <lgl>, venue_ref <chr>, team_ref <chr>,
#> #   franchise_ref <chr>
# }
```
