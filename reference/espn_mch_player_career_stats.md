# **Get ESPN MCH / WCH Player Career Stats (core-v2)**

**Get ESPN MCH / WCH Player Career Stats (core-v2)**

## Usage

``` r
espn_mch_player_career_stats(athlete_id, stat_type = NULL, ...)

espn_wch_player_career_stats(athlete_id, stat_type = NULL, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5136656"`).
  Obtain via
  [`espn_mch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_roster.md).

- stat_type:

  Optional integer stat-type path segment (default `NULL`, which uses
  the base `/statistics` path). ESPN redirects the base path to
  `/statistics/0` (All Splits). Pass `0` explicitly for identical
  behaviour.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with career aggregate statistics
flattened wide across all stat categories. Columns vary by player type
(skater vs goalie). Core columns include:

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | character | ESPN athlete identifier (echoed from arg). |
| split_id | character | Split identifier (e.g. "0" = All Splits). |
| split_name | character | Split name (e.g. "All Splits"). |
| split_type | character | Split type (e.g. "total"). |
| split_abbr | character | Split abbreviation (e.g. "TOTAL"). |
| offensive_goals | numeric | Career goals. |
| offensive_assists | numeric | Career assists. |
| offensive_points | numeric | Career points. |
| general_games | numeric | Career games played. |
| penalties_penalty_minutes | numeric | Career penalty minutes. |
| ... | numeric | Additional stat columns per category. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_career_stats.md)
(columns inherited); a collection may be empty.

`espn_wch_player_career_stats()` is the same wrapper for women's college
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
  try(espn_mch_player_career_stats(athlete_id = "5136656"))
#> ✖ 2026-11-09 22:50:32.745509: Invalid arguments or no ESPN mens-college-hockey player career stats for athlete 5136656 available!
#> ✖ Args: league = "mens-college-hockey", athlete_id = "5136656", stat_type = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_career_stats(athlete_id = "5263060"))
#> ✖ 2026-11-09 22:50:32.780766: Invalid arguments or no ESPN womens-college-hockey player career stats for athlete 5263060 available!
#> ✖ Args: league = "womens-college-hockey", athlete_id = "5263060", stat_type = NULL
#> ✖ Error: The API returned an error
#> data frame with 0 columns and 0 rows
# }
```
