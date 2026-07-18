# **NHL Records - Franchise Team Totals**

Returns per-team-name totals from the NHL Records API
(`https://records.nhl.com/site/api/franchise-team-totals`). Useful for
relocated franchises where multiple team names map to one franchise.

## Usage

``` r
nhl_records_franchise_team_totals(franchise_id = NULL, cayenne_exp = NULL)
```

## Arguments

- franchise_id:

  Optional integer franchise ID. When supplied it is translated into a
  `cayenneExp=franchiseId={franchise_id}` filter unless `cayenne_exp` is
  already provided.

- cayenne_exp:

  Optional Cayenne filter expression string. Takes precedence over
  `franchise_id` when both are supplied.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique record identifier. |
| active_franchise | integer | Indicator of whether the franchise is active. |
| active_team | logical | Indicator of whether the team is active. |
| cups | integer | Number of Stanley Cup championships. |
| first_season_id | integer | Season ID of the team's first season. |
| franchise_id | integer | Unique franchise identifier. |
| game_type_id | integer | Game type the totals belong to. |
| game_win_pctg | numeric | Game-winning percentage. |
| games_played | integer | Total games played. |
| goals_against | integer | Goals against. |
| goals_for | integer | Goals for. |
| home_losses | integer | Losses at home. |
| home_overtime_losses | integer | Overtime losses at home. |
| home_ties | integer | Ties at home. |
| home_wins | integer | Wins at home. |
| last_season_id | integer | Season ID of the team's last season. |
| losses | integer | Total losses. |
| overtime_losses | integer | Total overtime losses. |
| penalty_minutes | integer | Penalty minutes. |
| playoff_seasons | integer | Number of playoff seasons. |
| point_pctg | numeric | Points percentage. |
| points | integer | Total standings points. |
| road_losses | integer | Losses on the road. |
| road_overtime_losses | integer | Overtime losses on the road. |
| road_ties | integer | Ties on the road. |
| road_wins | integer | Wins on the road. |
| series_losses | integer | Playoff series losses. |
| series_played | integer | Playoff series played. |
| series_win_pctg | numeric | Playoff series win percentage. |
| series_wins | integer | Playoff series wins. |
| shootout_losses | integer | Shootout losses. |
| shootout_wins | integer | Shootout wins. |
| shutouts | integer | Shutouts recorded. |
| team_id | integer | Unique team identifier. |
| team_name | character | Team name. |
| ties | integer | Total ties. |
| tri_code | character | Team three-letter code. |
| wins | integer | Total wins. |

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_team_totals())
#> ── NHL Records Franchise Team Totals ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:48:14 UTC
#> # A tibble: 120 × 38
#>       id active_franchise active_team  cups first_season_id franchise_id
#>    <int>            <int> <lgl>       <int>           <int>        <int>
#>  1     1                1 TRUE            3        19821983           23
#>  2     2                1 TRUE            3        19821983           23
#>  3     3                1 TRUE            4        19721973           22
#>  4     4                1 TRUE            4        19721973           22
#>  5     5                1 TRUE            4        19261927           10
#>  6     6                1 TRUE            4        19261927           10
#>  7     7                1 TRUE            2        19671968           16
#>  8     8                1 TRUE            2        19671968           16
#>  9     9                1 TRUE            5        19671968           17
#> 10    10                1 TRUE            5        19671968           17
#> # ℹ 110 more rows
#> # ℹ 32 more variables: game_type_id <int>, game_win_pctg <dbl>,
#> #   games_played <int>, goals_against <int>, goals_for <int>,
#> #   home_losses <int>, home_overtime_losses <int>, home_ties <int>,
#> #   home_wins <int>, last_season_id <int>, losses <int>, overtime_losses <int>,
#> #   penalty_minutes <int>, playoff_seasons <int>, point_pctg <dbl>,
#> #   points <int>, road_losses <int>, road_overtime_losses <int>, …
# }
```
