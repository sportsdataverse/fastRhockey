# **Get ESPN MCH / WCH Tournaments (core-v2)**

**Get ESPN MCH / WCH Tournaments (core-v2)**

## Usage

``` r
espn_mch_tournaments(limit = 200, ...)

espn_wch_tournaments(limit = 200, ...)
```

## Arguments

- limit:

  Maximum number of tournaments to return (default `200`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per tournament reference. The
MCH tournaments endpoint is sparsely populated (typically 1 entry):

|               |           |                                                |
|---------------|-----------|------------------------------------------------|
| col_name      | types     | description                                    |
| ref           | character | `$ref` URL for the tournament object.          |
| tournament_id | character | ESPN tournament id parsed from the `$ref` URL. |
| count         | integer   | Total tournaments in the collection.           |
| page_count    | integer   | Total pages in the collection.                 |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_tournaments()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_tournaments.md)
(columns inherited); a collection may be empty.

`espn_wch_tournaments()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_tournaments())
#> ! ESPN MENS-COLLEGE-HOCKEY tournaments endpoint returned no items.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_tournaments())
#> ! ESPN WOMENS-COLLEGE-HOCKEY tournaments endpoint returned no items.
#> data frame with 0 columns and 0 rows
# }
```
