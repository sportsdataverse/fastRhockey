# **Get ESPN NHL Player Overview**

**Get ESPN NHL Player Overview**

## Usage

``` r
espn_nhl_player_overview(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5149125"`).
  Obtain via
  [`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).

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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_overview(athlete_id = "5149125"))
#> ── NHL Player Overview data from ESPN.com ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:46 UTC
#> # A tibble: 3 × 23
#>   athlete_id athlete_espn_id athlete_display_name athlete_short_name
#>   <chr>      <chr>           <chr>                <chr>             
#> 1 5149125    NA              NA                   NA                
#> 2 5149125    NA              NA                   NA                
#> 3 5149125    NA              NA                   NA                
#> # ℹ 19 more variables: athlete_jersey <chr>, athlete_position <chr>,
#> #   athlete_team_id <chr>, athlete_team_abbreviation <chr>,
#> #   split_display_name <chr>, games <chr>, goals <chr>, assists <chr>,
#> #   points <chr>, plus_minus <chr>, penalty_minutes <chr>, shots_total <chr>,
#> #   power_play_goals <chr>, power_play_assists <chr>, short_handed_goals <chr>,
#> #   short_handed_assists <chr>, game_winning_goals <chr>,
#> #   time_on_ice_per_game <chr>, production <chr>
# }
```
