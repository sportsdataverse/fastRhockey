# **NHL Player Stats**

Returns season-by-season stats for a given player ID. Uses the new NHL
API player landing endpoint (`api-web.nhle.com`).

**Note:** The original `statsapi.web.nhl.com` endpoint has been retired.
This function now returns the `seasonTotals` data from the player
landing page, which includes year-by-year stats across all leagues (NHL,
AHL, junior, etc.).

## Usage

``` r
nhl_player_stats(player_id)
```

## Arguments

- player_id:

  Player unique ID (e.g., 8476899)

## Value

Returns a data frame with season-by-season stats for the player,
including games played, goals, assists, and league-specific columns.

## Examples

``` r
# \donttest{
  try(nhl_player_stats(player_id = 8476899))
#> ── NHL Player Stats Information from NHL.com ────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:57:08 UTC
#> # A tibble: 37 × 41
#>    game_type_id games_played goals_against_avg league_abbrev save_pctg   season
#>           <int>        <int>             <dbl> <chr>             <dbl>    <int>
#>  1            2           32              5.92 Bantam            0.87  20072008
#>  2            2           29              2.84 Bantam            0     20082009
#>  3            2           40              1.71 Minor-ON         NA     20092010
#>  4            2           28              3.79 OHL              NA     20102011
#>  5            2           36              4.08 OHL              NA     20112012
#>  6            2            7              2.72 WJ18-A            0.910 20112012
#>  7            2           53              3.67 OHL               0.894 20122013
#>  8            3            6              2.67 OHL               0.910 20122013
#>  9            2           49              2.57 OHL               0.921 20132014
#> 10            2            1              2    AHL               0.92  20132014
#> # ℹ 27 more rows
#> # ℹ 35 more variables: sequence <int>, team_name_default <chr>,
#> #   goals_against <int>, losses <int>, shutouts <int>, ties <int>,
#> #   time_on_ice <chr>, wins <int>, shots_against <int>, team_name_cs <chr>,
#> #   team_name_de <chr>, team_name_es <chr>, team_name_fi <chr>,
#> #   team_name_sk <chr>, team_name_sv <chr>, team_common_name_default <chr>,
#> #   team_common_name_cs <chr>, team_common_name_de <chr>, …
# }
```
