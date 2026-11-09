# **Get ESPN MCH / WCH League Notes (core-v2)**

**Get ESPN MCH / WCH League Notes (core-v2)**

## Usage

``` r
espn_mch_league_notes(...)

espn_wch_league_notes(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per league note reference, or
an empty tibble when the endpoint is unavailable. **Note**: The MCH
league notes endpoint (`core_v2/notes`) currently returns `count=0` - an
empty tibble is returned gracefully:

|            |           |                                              |
|------------|-----------|----------------------------------------------|
| col_name   | types     | description                                  |
| ref        | character | `$ref` URL for the note object.              |
| note_id    | character | ESPN note id parsed from the `$ref` URL.     |
| count      | integer   | Total notes in the collection (typically 0). |
| page_count | integer   | Total pages in the collection.               |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_league_notes()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_league_notes.md)
(columns inherited); a collection may be empty.

`espn_wch_league_notes()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  # NOTE: Returns empty tibble for MCH (ESPN does not populate this endpoint)
  try(espn_mch_league_notes())
#> ! ESPN MENS-COLLEGE-HOCKEY league notes endpoint returned no items (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  # NOTE: Returns empty tibble for WCH (ESPN does not populate this endpoint)
  try(espn_wch_league_notes())
#> ! ESPN WOMENS-COLLEGE-HOCKEY league notes endpoint returned no items (count=0 is normal for NHL).
#> data frame with 0 columns and 0 rows
# }
```
