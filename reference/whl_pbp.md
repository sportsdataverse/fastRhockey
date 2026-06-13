# **WHL Play-by-Play**

WHL play-by-play for a single game (one row per event, fully enriched
with coordinate transforms, on-ice players, Corsi geometry).

## Usage

``` r
whl_pbp(game_id)
```

## Arguments

- game_id:

  Numeric WHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per play-by-play event.

## See also

Other WHL Functions:
[`most_recent_whl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_whl_season.md),
[`whl`](https://fastRhockey.sportsdataverse.org/reference/whl.md),
[`whl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_corsi.md),
[`whl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_shifts.md),
[`whl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/whl_game_summary.md),
[`whl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/whl_leaders.md),
[`whl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_stats.md),
[`whl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/whl_player_toi.md),
[`whl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/whl_schedule.md),
[`whl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/whl_season_id.md),
[`whl_standings()`](https://fastRhockey.sportsdataverse.org/reference/whl_standings.md),
[`whl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/whl_team_roster.md),
[`whl_teams()`](https://fastRhockey.sportsdataverse.org/reference/whl_teams.md)

## Examples

``` r
 try(whl_pbp(game_id = 27225)) 
#> ── WHL Play-by-Play from HockeyTech ─────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 02:50:51 UTC
#> # A tibble: 29 × 97
#>    game_id event team_id period_of_game time_of_period x_coord y_coord player_id
#>      <dbl> <chr> <chr>   <chr>          <chr>            <dbl>   <dbl>     <int>
#>  1   27225 goal… 201     1              0:00                NA      NA        NA
#>  2   27225 goal… 204     1              0:00                NA      NA        NA
#>  3   27225 goal  204     1              1:21                NA      NA     21582
#>  4   27225 pena… 201     1              6:35                NA      NA     22064
#>  5   27225 pena… 204     1              14:05               NA      NA     22199
#>  6   27225 pena… 201     1              14:53               NA      NA     22336
#>  7   27225 pena… 201     1              15:25               NA      NA     21964
#>  8   27225 pena… 204     1              17:35               NA      NA     23496
#>  9   27225 pena… 204     1              18:59               NA      NA     23492
#> 10   27225 pena… 201     1              18:59               NA      NA     22345
#> # ℹ 19 more rows
#> # ℹ 89 more variables: player_name_first <chr>, player_name_last <chr>,
#> #   player_position <chr>, goal <lgl>, goalie_id <int>, goalie_first <chr>,
#> #   goalie_last <chr>, empty_net <chr>, game_winner <chr>, penalty_shot <chr>,
#> #   insurance <chr>, short_handed <chr>, power_play <chr>,
#> #   plus_player_one_id <int>, plus_player_one_first <chr>,
#> #   plus_player_one_last <chr>, plus_player_one_position <chr>, …
```
