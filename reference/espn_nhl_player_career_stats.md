# **Get ESPN NHL Player Career Stats (core-v2)**

**Get ESPN NHL Player Career Stats (core-v2)**

## Usage

``` r
espn_nhl_player_career_stats(athlete_id, stat_type = NULL, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5149125"`).
  Obtain via
  [`espn_nhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_roster.md).

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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_career_stats(athlete_id = "5149125"))
#> ── NHL Player Career Stats data from ESPN core-v2 ───────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:42 UTC
#> # A tibble: 1 × 63
#>   athlete_id split_id split_name split_type split_abbr defensive_goals_against
#>   <chr>      <chr>    <chr>      <chr>      <chr>                        <dbl>
#> 1 5149125    0        All Splits total      TOTAL                            0
#> # ℹ 57 more variables: defensive_avg_goals_against <dbl>,
#> #   defensive_shots_against <dbl>, defensive_avg_shots_against <dbl>,
#> #   defensive_shootout_saves <dbl>, defensive_shootout_shots_against <dbl>,
#> #   defensive_shootout_save_pct <dbl>, defensive_empty_net_goals_against <dbl>,
#> #   defensive_shutouts <dbl>, defensive_saves <dbl>, defensive_save_pct <dbl>,
#> #   defensive_overtime_losses <dbl>, defensive_blocked_shots <dbl>,
#> #   defensive_hits <dbl>, defensive_even_strength_saves <dbl>, …
# }
```
