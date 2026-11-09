# **Get ESPN MCH / WCH Injuries**

**Get ESPN MCH / WCH Injuries**

## Usage

``` r
espn_mch_injuries(...)

espn_wch_injuries(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per injured player:

|  |  |  |
|----|----|----|
| col_name | types | description |
| team_id | character | ESPN team identifier. |
| team_display_name | character | Team display name. |
| player_id | character | ESPN player identifier. |
| player_display_name | character | Player display name. |
| player_short_name | character | Player short name. |
| player_position | character | Player position abbreviation. |
| injury_id | character | ESPN injury identifier. |
| injury_status | character | Injury status (e.g. "Questionable", "Out"). |
| injury_date | character | Injury date (ISO 8601). |
| injury_short_comment | character | Short injury comment. |
| injury_long_comment | character | Long injury comment. |
| injury_type | character | Injury type name. |
| injury_abbreviation | character | Injury type abbreviation. |
| detail_side | character | Injured side (e.g. "Left", "Right"). |
| detail_fantasy_status | character | Fantasy relevance status. |
| detail_return_date | character | Projected return date. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_injuries()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_injuries.md);
ESPN answered an empty collection for every captured MCH body (2026-10),
so the columns below are the NHL helper's and unverified for this
league.

`espn_wch_injuries()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_injuries())
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_injuries())
#> data frame with 0 columns and 0 rows
# }
```
