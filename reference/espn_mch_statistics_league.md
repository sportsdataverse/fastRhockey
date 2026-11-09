# **Get ESPN MCH / WCH League Statistics**

**Get ESPN MCH / WCH League Statistics**

## Usage

``` r
espn_mch_statistics_league(...)

espn_wch_statistics_league(...)
```

## Arguments

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per (stat category, leader).
The ESPN MCH statistics endpoint returns a sparse set of top-of-league
stat leaders organized by category. If the endpoint returns no
categories, an empty tibble is returned gracefully:

|  |  |  |
|----|----|----|
| col_name | types | description |
| category_name | character | Stat category name (e.g. "goals", "assists"). |
| category_abbreviation | character | Category abbreviation. |
| display_value | character | Leader stat display value. |
| value | numeric | Leader stat numeric value. |
| athlete_id | character | ESPN athlete identifier. |
| athlete_display_name | character | Athlete display name. |
| team_id | character | ESPN team identifier. |
| team_abbreviation | character | Team abbreviation. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_statistics_league()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_statistics_league.md)
(columns inherited); a collection may be empty.

`espn_wch_statistics_league()` is the same wrapper for women's college
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
  try(espn_mch_statistics_league())
#> ! ESPN MENS-COLLEGE-HOCKEY statistics endpoint returned no stat categories.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_statistics_league())
#> ! ESPN WOMENS-COLLEGE-HOCKEY statistics endpoint returned no stat categories.
#> data frame with 0 columns and 0 rows
# }
```
