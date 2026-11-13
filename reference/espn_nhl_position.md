# **Get ESPN NHL Position (core-v2)**

**Get ESPN NHL Position (core-v2)**

## Usage

``` r
espn_nhl_position(position_id, ...)
```

## Arguments

- position_id:

  ESPN position identifier (character or numeric). Use
  [`espn_nhl_positions()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_positions.md)
  to discover valid position ids.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with position detail:

|              |           |                                               |
|--------------|-----------|-----------------------------------------------|
| col_name     | types     | description                                   |
| position_id  | character | ESPN position id (echoed from arg).           |
| id           | character | ESPN position identifier from response.       |
| name         | character | Position name (e.g. "Center").                |
| display_name | character | Position display name.                        |
| abbreviation | character | Position abbreviation (e.g. "C").             |
| leaf         | logical   | Whether this is a leaf (non-parent) position. |
| parent_ref   | character | `$ref` URL for the parent position (if any).  |
| position_ref | character | `$ref` URL for this position object.          |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    pos <- espn_nhl_positions()
    espn_nhl_position(position_id = pos$position_id[1])
  })
#> ── NHL Position data from ESPN core-v2 ──────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:27:17 UTC
#> # A tibble: 1 × 8
#>   position_id id    name   display_name abbreviation leaf  parent_ref           
#>   <chr>       <chr> <chr>  <chr>        <chr>        <lgl> <chr>                
#> 1 1           1     Center Center       C            TRUE  http://sports.core.a…
#> # ℹ 1 more variable: position_ref <chr>
# }
```
