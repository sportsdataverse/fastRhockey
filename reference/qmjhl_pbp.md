# **QMJHL Play-by-Play**

QMJHL play-by-play for a single game (one row per event, fully enriched
with coordinate transforms, on-ice players, Corsi geometry).

## Usage

``` r
qmjhl_pbp(game_id)
```

## Arguments

- game_id:

  Numeric QMJHL game identifier.

## Value

A `fastRhockey_data` data frame, one row per play-by-play event.

## See also

Other QMJHL Functions:
[`most_recent_qmjhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_qmjhl_season.md),
[`qmjhl`](https://fastRhockey.sportsdataverse.org/reference/qmjhl.md),
[`qmjhl_game_corsi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_corsi.md),
[`qmjhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_shifts.md),
[`qmjhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_game_summary.md),
[`qmjhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_leaders.md),
[`qmjhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_stats.md),
[`qmjhl_player_toi()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_player_toi.md),
[`qmjhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_schedule.md),
[`qmjhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_season_id.md),
[`qmjhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_standings.md),
[`qmjhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_team_roster.md),
[`qmjhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/qmjhl_teams.md)

## Examples

``` r
 try(qmjhl_pbp(game_id = 27225)) 
#> ── QMJHL Play-by-Play from HockeyTech ───────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 14:21:27 UTC
#> # A tibble: 193 × 100
#>    game_id event team_id period_of_game time_of_period x_coord y_coord player_id
#>      <dbl> <chr> <chr>   <chr>          <chr>            <dbl>   <dbl>     <int>
#>  1   27225 goal… 14      1              0:00              NA      NA          NA
#>  2   27225 goal… 60      1              0:00              NA      NA          NA
#>  3   27225 face… NA      1              0:00               0       0       14809
#>  4   27225 shot  60      1              0:09              32.7   -23.0     16995
#>  5   27225 face… NA      1              0:09              52.3    28.6     14809
#>  6   27225 shot  60      1              0:20              55.7   -16.2     16496
#>  7   27225 face… NA      1              0:20              52.3   -15.9     14696
#>  8   27225 hit   60      1              0:29             -41      40.0     16515
#>  9   27225 face… NA      1              0:29              52.3   -15.9     14696
#> 10   27225 shot  14      1              1:40             -66.7   -12.8     17001
#> # ℹ 183 more rows
#> # ℹ 92 more variables: player_name_first <chr>, player_name_last <chr>,
#> #   player_position <chr>, goal <lgl>, goalie_id <int>, goalie_first <chr>,
#> #   goalie_last <chr>, home_win <chr>, player_team_id <chr>, event_type <chr>,
#> #   shot_quality <chr>, player_two_id <int>, player_two_name_first <chr>,
#> #   player_two_name_last <chr>, player_two_position <chr>,
#> #   penalty_length <chr>, power_play <chr>, empty_net <chr>, …
```
