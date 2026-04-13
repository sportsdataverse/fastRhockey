# **NHL Records - Franchise Totals**

Returns all-time franchise totals from the NHL Records API
(`https://records.nhl.com/site/api/franchise-totals`). Optionally filter
by franchise ID or a `cayenneExp` expression.

## Usage

``` r
nhl_records_franchise_totals(franchise_id = NULL, cayenne_exp = NULL)
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

A `fastRhockey_data` tibble of franchise totals, or `NULL` on failure.

## Examples

``` r
# \donttest{
  try(nhl_records_franchise_totals())
#> ── NHL Records Franchise Totals ─────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-13 17:05:38 UTC
#> # A tibble: 80 × 37
#>       id active_franchise  cups first_season_id franchise_id game_type_id
#>    <int>            <int> <int>           <int>        <int>        <int>
#>  1     1                0     3        19791980           27            3
#>  2     2                1     1        19741975           24            3
#>  3     3                0     0        19191920            4            2
#>  4     4                0     2        19241925            7            2
#>  5     5                1     0        19921993           30            3
#>  6     6                1     4        19261927           10            2
#>  7     7                1     1        19671968           18            3
#>  8     8                0     0        19671968           13            2
#>  9     9                1     0        20212022           39            3
#> 10    10                1     2        19931994           33            2
#> # ℹ 70 more rows
#> # ℹ 31 more variables: game_win_pctg <dbl>, games_played <int>,
#> #   goals_against <int>, goals_for <int>, home_losses <int>,
#> #   home_overtime_losses <int>, home_ties <int>, home_wins <int>,
#> #   last_season_id <int>, losses <int>, overtime_losses <int>,
#> #   penalty_minutes <int>, playoff_seasons <int>, point_pctg <dbl>,
#> #   points <int>, road_losses <int>, road_overtime_losses <int>, …
# }
```
