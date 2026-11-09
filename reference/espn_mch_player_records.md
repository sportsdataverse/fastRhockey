# **Get ESPN MCH / WCH Player Records (core-v2)**

**Get ESPN MCH / WCH Player Records (core-v2)**

## Usage

``` r
espn_mch_player_records(athlete_id, ...)

espn_wch_player_records(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per record, or an empty data
frame if no records are served. As of 2026-06-08, ESPN core-v2 returns
`count=0` for all queried NHL players (DONE_WITH_CONCERNS: endpoint
exists but data is not populated).

|            |           |                                             |
|------------|-----------|---------------------------------------------|
| col_name   | types     | description                                 |
| ref        | character | `$ref` URL for the record object.           |
| record_id  | character | Record id parsed from the `$ref` URL.       |
| athlete_id | character | ESPN athlete id (echoed from arg).          |
| count      | integer   | Total record count (0 in practice for NHL). |
| page_count | integer   | Total page count.                           |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_records()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_records.md)
(columns inherited); a collection may be empty.

`espn_wch_player_records()` is the same wrapper for women's college
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
  try(espn_mch_player_records(athlete_id = "5136656"))
#> ! espn_nhl_player_records(): No record data served by ESPN core-v2 for athlete 5136656. The endpoint responds with count=0 for all queried NHL players (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_records(athlete_id = "5263060"))
#> ! espn_nhl_player_records(): No record data served by ESPN core-v2 for athlete 5263060. The endpoint responds with count=0 for all queried NHL players (DONE_WITH_CONCERNS: endpoint exists but data is not populated).
#> data frame with 0 columns and 0 rows
# }
```
