# **QMJHL Schedule**

QMJHL schedule from the HockeyTech feed (one row per game).

## Usage

``` r
qmjhl_schedule(season = NULL, season_id = NULL)
```

## Arguments

- season:

  End-year season (e.g. 2025); optional.

- season_id:

  Explicit HockeyTech season id; optional.

## Value

A `fastRhockey_data` data frame, one row per game.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_pbp.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_schedule()) 
#> ── QMJHL Schedule from HockeyTech ───────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-24 02:07:14 UTC
#> # A tibble: 10,000 × 12
#>    game_id game_date     game_status home_team home_team_id home_score away_team
#>    <chr>   <chr>         <chr>       <chr>     <chr>        <chr>      <chr>    
#>  1 5025    1969-10-05T0… Final       Drummond… 47           3          Cornwall…
#>  2 5089    1969-10-05T0… Final 1st … Verdun, … 50           5          St-Jérôm…
#>  3 5110    1969-10-05T0… Final 1st … Laval, S… 51           4          Québec, …
#>  4 5185    1969-10-05T0… Final       Sherbroo… 18           1          Sorel, É…
#>  5 5213    1969-10-05T0… Final       Shawinig… 13           11         Trois-Ri…
#>  6 5156    1969-10-06T0… Final       Laval, S… 51           5          Trois-Ri…
#>  7 5085    1969-10-07T0… Final       Verdun, … 50           11         Laval, S…
#>  8 5149    1969-10-07T0… Final 1st … St-Jérôm… 48           5          Sorel, É…
#>  9 5237    1969-10-07T0… Final       Trois-Ri… 4            3          Québec, …
#> 10 5053    1969-10-09T0… Final       Cornwall… 49           5          Verdun, …
#> # ℹ 9,990 more rows
#> # ℹ 5 more variables: away_team_id <chr>, away_score <chr>, venue <chr>,
#> #   season_id <chr>, game_type <chr>
```
