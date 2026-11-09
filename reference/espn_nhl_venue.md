# **Get ESPN NHL Venue (core-v2)**

**Get ESPN NHL Venue (core-v2)**

## Usage

``` r
espn_nhl_venue(venue_id, ...)
```

## Arguments

- venue_id:

  ESPN venue identifier (character or numeric). Use
  [`espn_nhl_venues()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_venues.md)
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
| capacity  | integer   | Seating capacity (if returned; often `NA` for NHL). |
| venue_ref | character | `$ref` URL for this venue object.                   |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    vns <- espn_nhl_venues()
    espn_nhl_venue(venue_id = vns$venue_id[1])
  })
#> ── NHL Venue data from ESPN core-v2 ─────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:50:16 UTC
#> # A tibble: 1 × 11
#>   venue_id id    guid  full_name     city   state country grass indoor capacity
#>   <chr>    <chr> <chr> <chr>         <chr>  <chr> <chr>   <lgl> <lgl>     <int>
#> 1 1        1     NA    Boston Garden Boston MA    USA     FALSE FALSE        NA
#> # ℹ 1 more variable: venue_ref <chr>
# }
```
