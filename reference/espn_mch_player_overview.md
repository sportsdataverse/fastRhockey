# **Get ESPN MCH / WCH Player Overview**

**Get ESPN MCH / WCH Player Overview**

## Usage

``` r
espn_mch_player_overview(athlete_id, ...)

espn_wch_player_overview(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5136656"`).
  Obtain via
  [`espn_mch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_roster.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per stats split (Regular Season
/ Career):

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | character | ESPN athlete identifier (echoed from arg). |
| athlete_espn_id | character | ESPN athlete identifier (from payload). |
| athlete_display_name | character | Player display name. |
| athlete_short_name | character | Player short name. |
| athlete_jersey | character | Jersey number. |
| athlete_position | character | Position abbreviation. |
| athlete_team_id | character | ESPN team identifier. |
| athlete_team_abbreviation | character | Team abbreviation. |
| split_display_name | character | Split label (e.g. Regular Season, Career). |
| games | character | Games played. |
| goals | character | Goals. |
| assists | character | Assists. |
| points | character | Points. |
| plus_minus | character | Plus/minus. |
| penalty_minutes | character | Penalty minutes. |
| shots_total | character | Shots on goal. |
| power_play_goals | character | Power-play goals. |
| power_play_assists | character | Power-play assists. |
| short_handed_goals | character | Short-handed goals. |
| short_handed_assists | character | Short-handed assists. |
| game_winning_goals | character | Game-winning goals. |
| time_on_ice_per_game | character | Average time on ice per game. |
| production | character | Production metric. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_overview()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_overview.md)
(columns inherited); a collection may be empty.

`espn_wch_player_overview()` is the same wrapper for women's college
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
  try(espn_mch_player_overview(athlete_id = "5136656"))
#> ── MENS-COLLEGE-HOCKEY Player Overview data from ESPN.com ──────────────────────
#> ℹ Data updated: 2026-11-13 21:26:25 UTC
#> # A tibble: 1 × 8
#>   athlete_id athlete_espn_id athlete_display_name athlete_short_name
#>   <chr>      <chr>           <chr>                <chr>             
#> 1 5136656    NA              NA                   NA                
#> # ℹ 4 more variables: athlete_jersey <chr>, athlete_position <chr>,
#> #   athlete_team_id <chr>, athlete_team_abbreviation <chr>
# }
# \donttest{
  try(espn_wch_player_overview(athlete_id = "5263060"))
#> ── WOMENS-COLLEGE-HOCKEY Player Overview data from ESPN.com ────────────────────
#> ℹ Data updated: 2026-11-13 21:26:25 UTC
#> # A tibble: 1 × 8
#>   athlete_id athlete_espn_id athlete_display_name athlete_short_name
#>   <chr>      <chr>           <chr>                <chr>             
#> 1 5263060    NA              NA                   NA                
#> # ℹ 4 more variables: athlete_jersey <chr>, athlete_position <chr>,
#> #   athlete_team_id <chr>, athlete_team_abbreviation <chr>
# }
```
