# **Get ESPN NHL Player Statistics (core-v2)**

**Get ESPN NHL Player Statistics (core-v2)**

## Usage

``` r
espn_nhl_player_statistics(athlete_id, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with career aggregate statistics.
Columns vary by player type (skater vs goalie). Core columns include:

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | character | ESPN athlete id (echoed from arg). |
| split_id | character | Split identifier (e.g. "0" for all splits). |
| split_name | character | Split name (e.g. "All Splits"). |
| split_type | character | Split type (e.g. "total"). |
| split_abbr | character | Split abbreviation (e.g. "TOTAL"). |
| goals | numeric | Career goals (skaters) or goals-against (goalies). |
| assists | numeric | Career assists (skaters). |
| points | numeric | Career points. |
| games_played | numeric | Career games played. |
| ... | numeric | Additional stat columns flattened from all categories. |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_statistics(athlete_id = "4024820"))
#> ── NHL Player Statistics data from ESPN core-v2 ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:23 UTC
#> # A tibble: 1 × 60
#>   athlete_id split_id split_name split_type split_abbr goals_against
#>   <chr>      <chr>    <chr>      <chr>      <chr>              <dbl>
#> 1 4024820    0        All Splits total      TOTAL                  0
#> # ℹ 54 more variables: avg_goals_against <dbl>, shots_against <dbl>,
#> #   avg_shots_against <dbl>, shootout_saves <dbl>,
#> #   shootout_shots_against <dbl>, shootout_save_pct <dbl>,
#> #   empty_net_goals_against <dbl>, shutouts <dbl>, saves <dbl>, save_pct <dbl>,
#> #   overtime_losses <dbl>, blocked_shots <dbl>, hits <dbl>,
#> #   even_strength_saves <dbl>, power_play_saves <dbl>,
#> #   short_handed_saves <dbl>, games <dbl>, game_started <dbl>, …
# }
```
