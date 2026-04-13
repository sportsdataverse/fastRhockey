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

A `fastRhockey_data` tibble of franchise team totals, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_team_totals())
#> ── NHL Records Franchise Team Totals ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:38 UTC
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
