# **Get ESPN MCH / WCH Franchise (core-v2)**

**Get ESPN MCH / WCH Franchise (core-v2)**

## Usage

``` r
espn_mch_franchise(franchise_id, ...)

espn_wch_franchise(franchise_id, ...)
```

## Arguments

- franchise_id:

  ESPN franchise identifier (character or numeric). Use
  [`espn_mch_franchises()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_franchises.md)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_franchise()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_franchise.md)
(columns inherited); a collection may be empty.

`espn_wch_franchise()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    fcs <- espn_mch_franchises()
    espn_mch_franchise(franchise_id = fcs$franchise_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Franchise data from ESPN core-v2 ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:50:14 UTC
#> # A tibble: 1 × 15
#>   franchise_id id    uid           slug     location name  nickname abbreviation
#>   <chr>        <chr> <chr>         <chr>    <chr>    <chr> <chr>    <chr>       
#> 1 1            1     s:70~l:91~f:1 alaska-… Alaska … Seaw… AK-Anch… UAA         
#> # ℹ 7 more variables: display_name <chr>, short_display_name <chr>,
#> #   color <chr>, is_active <lgl>, venue_ref <chr>, team_ref <chr>,
#> #   franchise_ref <chr>
# }
# \donttest{
  try({
    fcs <- espn_wch_franchises()
    espn_wch_franchise(franchise_id = fcs$franchise_id[1])
  })
#> ── WOMENS-COLLEGE-HOCKEY Franchise data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 22:50:14 UTC
#> # A tibble: 1 × 15
#>   franchise_id id    uid            slug    location name  nickname abbreviation
#>   <chr>        <chr> <chr>          <chr>   <chr>    <chr> <chr>    <chr>       
#> 1 41           41    s:70~l:92~f:41 uconn-… UConn    Husk… UConn    CONN        
#> # ℹ 7 more variables: display_name <chr>, short_display_name <chr>,
#> #   color <chr>, is_active <lgl>, venue_ref <chr>, team_ref <chr>,
#> #   franchise_ref <chr>
# }
```
