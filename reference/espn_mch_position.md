# **Get ESPN MCH / WCH Position (core-v2)**

**Get ESPN MCH / WCH Position (core-v2)**

## Usage

``` r
espn_mch_position(position_id, ...)

espn_wch_position(position_id, ...)
```

## Arguments

- position_id:

  ESPN position identifier (character or numeric). Use
  [`espn_mch_positions()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_positions.md)
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_position()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_position.md)
(columns inherited); a collection may be empty.

`espn_wch_position()` is the same wrapper for women's college hockey
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
    pos <- espn_mch_positions()
    espn_mch_position(position_id = pos$position_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Position data from ESPN core-v2 ──── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:27 UTC
#> # A tibble: 1 × 8
#>   position_id id    name   display_name abbreviation leaf  parent_ref           
#>   <chr>       <chr> <chr>  <chr>        <chr>        <lgl> <chr>                
#> 1 1           1     Center Center       C            TRUE  http://sports.core.a…
#> # ℹ 1 more variable: position_ref <chr>
# }
# \donttest{
  try({
    pos <- espn_wch_positions()
    espn_wch_position(position_id = pos$position_id[1])
  })
#> ── WOMENS-COLLEGE-HOCKEY Position data from ESPN core-v2 ── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 21:26:27 UTC
#> # A tibble: 1 × 8
#>   position_id id    name   display_name abbreviation leaf  parent_ref           
#>   <chr>       <chr> <chr>  <chr>        <chr>        <lgl> <chr>                
#> 1 1           1     Center Center       C            TRUE  http://sports.core.a…
#> # ℹ 1 more variable: position_ref <chr>
# }
```
