# **Load NHL team box scores (alias)**

Alias of
[`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md)
for naming parity with sportsdataverse-py.

## Usage

``` r
load_nhl_team_boxscores(seasons = most_recent_nhl_season(), ...)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

## Value

See
[`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md).

## See also

[`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md)

Other NHL Loader Functions:
[`load_nhl_goalie_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_boxscores.md),
[`load_nhl_pbp_full()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_full.md),
[`load_nhl_player_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscore.md),
[`load_nhl_player_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_boxscores.md),
[`load_nhl_schedules()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedules.md),
[`load_nhl_skater_boxscores()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_boxscores.md),
[`load_nhl_team_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_boxscore.md)

## Examples

``` r
# \donttest{
  try(load_nhl_team_boxscores(2022))
#> ── NHL Team Boxscores from fastRhockey data repository ──── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 06:01:44 UTC
#> # A tibble: 2,802 × 16
#>    home_away team_id team_abbrev team_name      goals shots_on_goal   pim  hits
#>    <chr>       <int> <chr>       <chr>          <int>         <int> <int> <int>
#>  1 away            5 PIT         Penguins           6            35     2    28
#>  2 home           14 TBL         Lightning          2            28     0    31
#>  3 away           55 SEA         Kraken             3            31     6    33
#>  4 home           54 VGK         Golden Knights     4            30     8    26
#>  5 away            8 MTL         Canadiens          1            32     6    32
#>  6 home           10 TOR         Maple Leafs        2            30     8    14
#>  7 away            3 NYR         Rangers            1            24    18    27
#>  8 home           15 WSH         Capitals           5            27    16    12
#>  9 away           16 CHI         Blackhawks         2            34    10    17
#> 10 home           21 COL         Avalanche          4            36    12    25
#> # ℹ 2,792 more rows
#> # ℹ 8 more variables: blocked_shots <int>, giveaways <int>, takeaways <int>,
#> #   power_play_goals <int>, faceoff_win_pctg <dbl>, saves <int>,
#> #   save_pctg <dbl>, goals_against <int>
# }
```
