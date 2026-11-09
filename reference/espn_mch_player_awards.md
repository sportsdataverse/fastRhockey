# **Get ESPN MCH / WCH Player Awards (core-v2)**

**Get ESPN MCH / WCH Player Awards (core-v2)**

## Usage

``` r
espn_mch_player_awards(athlete_id, ...)

espn_wch_player_awards(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per award, or an empty data
frame if the athlete has no award entries. Each `$ref` follows to a
season-award object (e.g. Hart Trophy, Conn Smythe, etc.).

|            |           |                                      |
|------------|-----------|--------------------------------------|
| col_name   | types     | description                          |
| ref        | character | `$ref` URL for the award object.     |
| award_id   | character | Award id parsed from the `$ref` URL. |
| athlete_id | character | ESPN athlete id (echoed from arg).   |
| count      | integer   | Total award count.                   |
| page_count | integer   | Total page count.                    |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_awards()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_awards.md)
(columns inherited); a collection may be empty.

`espn_wch_player_awards()` is the same wrapper for women's college
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
  try(espn_mch_player_awards(athlete_id = "5136656"))
#> ! No award data found for athlete 5136656.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_awards(athlete_id = "5263060"))
#> ! No award data found for athlete 5263060.
#> data frame with 0 columns and 0 rows
# }
```
