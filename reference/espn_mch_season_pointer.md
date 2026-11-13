# **Get ESPN MCH / WCH Current Season Pointer (core-v2)**

**Get ESPN MCH / WCH Current Season Pointer (core-v2)**

## Usage

``` r
espn_mch_season_pointer(...)

espn_wch_season_pointer(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with the current season metadata:

|                   |           |                                                |
|-------------------|-----------|------------------------------------------------|
| col_name          | types     | description                                    |
| season            | integer   | Current season year (e.g. 2026).               |
| start_date        | character | Season start date (ISO 8601).                  |
| end_date          | character | Season end date (ISO 8601).                    |
| display_name      | character | Season display name (e.g. "2025-26").          |
| current_type_id   | character | Current season-type identifier.                |
| current_type      | integer   | Current season-type code (1=pre,2=reg,3=post). |
| current_type_name | character | Current season-type name.                      |
| current_type_slug | character | Current season-type slug.                      |
| season_ref        | character | `$ref` URL for the full season object.         |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_pointer()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_pointer.md)
(columns inherited); a collection may be empty.

`espn_wch_season_pointer()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_season_pointer())
#> ── MENS-COLLEGE-HOCKEY Current Season Pointer data from ESPN core-v2 ───────────
#> ℹ Data updated: 2026-11-13 20:16:11 UTC
#> # A tibble: 1 × 9
#>   season start_date        end_date    display_name current_type_id current_type
#>    <int> <chr>             <chr>       <chr>        <chr>                  <int>
#> 1   2027 2026-09-01T07:00Z 2027-05-01… 2026-27      2                          2
#> # ℹ 3 more variables: current_type_name <chr>, current_type_slug <chr>,
#> #   season_ref <chr>
# }
# \donttest{
  try(espn_wch_season_pointer())
#> ── WOMENS-COLLEGE-HOCKEY Current Season Pointer data from ESPN core-v2 ─────────
#> ℹ Data updated: 2026-11-13 20:16:11 UTC
#> # A tibble: 1 × 9
#>   season start_date        end_date    display_name current_type_id current_type
#>    <int> <chr>             <chr>       <chr>        <chr>                  <int>
#> 1   2027 2026-09-01T07:00Z 2027-04-05… 2026-27      2                          2
#> # ℹ 3 more variables: current_type_name <chr>, current_type_slug <chr>,
#> #   season_ref <chr>
# }
```
