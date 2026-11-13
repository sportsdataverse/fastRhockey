# **Get ESPN MCH / WCH Power Index (fitt-v3 FPI)**

**Get ESPN MCH / WCH Power Index (fitt-v3 FPI)**

## Usage

``` r
espn_mch_fpi(season = NULL, limit = NULL, page = NULL, ...)

espn_wch_fpi(season = NULL, limit = NULL, page = NULL, ...)
```

## Arguments

- season:

  Season (4-digit END year) whose table to return; `NULL` (the default)
  returns the current season.

- limit:

  Page size. `NULL` (the default) leaves ESPN's default.

- page:

  Page number. `NULL` (the default) leaves ESPN's default.

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per team:

|                         |           |                                          |
|-------------------------|-----------|------------------------------------------|
| col_name                | types     | description                              |
| team_id                 | character | ESPN team identifier.                    |
| team_uid                | character | ESPN team UID.                           |
| team_abbreviation       | character | Team abbreviation.                       |
| team_display_name       | character | Team display name.                       |
| team_short_display_name | character | Team short display name.                 |
| team_nickname           | character | Team nickname.                           |
| season                  | integer   | Requested season (END year).             |
| ...                     | varies    | One column per FPI field in the payload. |

## Details

League slug `mens-college-hockey`. One row per team with a column per
FPI field, snake-cased from the payload's `categories` labels (zipped by
position and length-guarded); a zero-row `data.frame` when ESPN
publishes no table for men's college hockey. Mirrors sportsdataverse-py
`espn_mch_fpi()`.

`espn_wch_fpi()` is the same wrapper for women's college hockey (league
slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_fpi(season = 2026))
#> ✖ 2026-11-13 21:26:03.325786: Invalid arguments or no ESPN mens-college-hockey power index available!
#> ✖ Args: league = "mens-college-hockey", kind = "fpi", path = NULL, query = list(season = 2026)
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_fpi(season = 2026))
#> ✖ 2026-11-13 21:26:03.371397: Invalid arguments or no ESPN womens-college-hockey power index available!
#> ✖ Args: league = "womens-college-hockey", kind = "fpi", path = NULL, query = list(season = 2026)
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
