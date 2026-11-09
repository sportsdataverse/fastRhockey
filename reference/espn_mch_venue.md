# **Get ESPN MCH / WCH Venue (core-v2)**

**Get ESPN MCH / WCH Venue (core-v2)**

## Usage

``` r
espn_mch_venue(venue_id, ...)

espn_wch_venue(venue_id, ...)
```

## Arguments

- venue_id:

  ESPN venue identifier (character or numeric). Use
  [`espn_mch_venues()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_venues.md)
  to discover valid venue ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with venue detail:

|           |           |                                                     |
|-----------|-----------|-----------------------------------------------------|
| col_name  | types     | description                                         |
| venue_id  | character | ESPN venue id (echoed from arg).                    |
| id        | character | ESPN venue identifier from response.                |
| guid      | character | Venue global unique identifier (if present).        |
| full_name | character | Venue full name.                                    |
| city      | character | Venue city.                                         |
| state     | character | Venue state/province abbreviation.                  |
| country   | character | Venue country.                                      |
| grass     | logical   | Whether the surface is grass.                       |
| indoor    | logical   | Whether the venue is indoors.                       |
| capacity  | integer   | Seating capacity (if returned; often `NA` for MCH). |
| venue_ref | character | `$ref` URL for this venue object.                   |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_venue()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_venue.md)
(columns inherited); a collection may be empty.

`espn_wch_venue()` is the same wrapper for women's college hockey
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
    vns <- espn_mch_venues()
    espn_mch_venue(venue_id = vns$venue_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Venue data from ESPN core-v2 ─────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:57 UTC
#> # A tibble: 1 × 11
#>   venue_id id    guid  full_name       city  state country grass indoor capacity
#>   <chr>    <chr> <chr> <chr>           <chr> <chr> <chr>   <lgl> <lgl>     <int>
#> 1 14       14    NA    Harold Alfond … Orono ME    USA     FALSE TRUE         NA
#> # ℹ 1 more variable: venue_ref <chr>
# }
# \donttest{
  try({
    vns <- espn_wch_venues()
    espn_wch_venue(venue_id = vns$venue_id[1])
  })
#> ── WOMENS-COLLEGE-HOCKEY Venue data from ESPN core-v2 ───── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:57 UTC
#> # A tibble: 1 × 11
#>   venue_id id    guid  full_name       city  state country grass indoor capacity
#>   <chr>    <chr> <chr> <chr>           <chr> <chr> <chr>   <lgl> <lgl>     <int>
#> 1 14       14    NA    Harold Alfond … Orono ME    USA     FALSE TRUE         NA
#> # ℹ 1 more variable: venue_ref <chr>
# }
```
