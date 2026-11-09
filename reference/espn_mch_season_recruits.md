# **Get ESPN MCH / WCH Season Recruits (core-v2)**

**Get ESPN MCH / WCH Season Recruits (core-v2)**

## Usage

``` r
espn_mch_season_recruits(season, limit = 1000, page = 1, ...)

espn_wch_season_recruits(season, limit = 1000, page = 1, ...)
```

## Arguments

- season:

  Season END year (e.g. `2026`).

- limit:

  Maximum number of items per page. Defaults to `1000`.

- page:

  Page number. Defaults to `1`.

- ...:

  Passed through to
  [`.retry_request()`](https://fastRhockey.sportsdataverse.org/reference/dot-retry_request.md)
  (e.g. `proxy =`).

## Value

A `fastRhockey_data` tibble with one row per recruit:

|            |           |                                    |
|------------|-----------|------------------------------------|
| col_name   | types     | description                        |
| ref        | character | `$ref` URL of the recruit object.  |
| recruit_id | character | Trailing id of the recruit `$ref`. |
| season     | integer   | Season END year (echoed from arg). |

## Details

League slug `mens-college-hockey`. Core-v2 `seasons/{season}/recruits`;
`$ref`-only items become `ref` + `recruit_id`, inline items are
flattened. Mirrors sportsdataverse-py `espn_mch_season_recruits()`; ESPN
may publish nothing for men's college hockey.

`espn_wch_season_recruits()` is the same wrapper for women's college
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
  try(espn_mch_season_recruits(season = 2026))
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_season_recruits(season = 2026))
#> data frame with 0 columns and 0 rows
# }
```
