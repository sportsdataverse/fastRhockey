# **Get ESPN MCH / WCH Recruiting Players (core-v2)**

**Get ESPN MCH / WCH Recruiting Players (core-v2)**

## Usage

``` r
espn_mch_recruiting_players(year, limit = 1000, page = 1, ...)

espn_wch_recruiting_players(year, limit = 1000, page = 1, ...)
```

## Arguments

- year:

  Recruiting class year.

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

|            |           |                                                  |
|------------|-----------|--------------------------------------------------|
| col_name   | types     | description                                      |
| ref        | character | `$ref` URL of the recruit (when ref-only).       |
| recruit_id | character | Trailing id of the `$ref` (when ref-only).       |
| year       | integer   | Recruiting class year (echoed from arg).         |
| ...        | varies    | Flattened athlete fields when ESPN inlines them. |

## Details

League slug `mens-college-hockey`. Core-v2 `recruiting/{year}/athletes`;
inline athlete objects are flattened and snake-cased, `$ref`-only items
become `ref` + `recruit_id`. Mirrors sportsdataverse-py
`espn_mch_recruiting_players()`.

`espn_wch_recruiting_players()` is the same wrapper for women's college
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
  try(espn_mch_recruiting_players(year = 2026))
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_recruiting_players(year = 2026))
#> data frame with 0 columns and 0 rows
# }
```
