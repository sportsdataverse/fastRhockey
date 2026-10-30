# VHL Skater Stats

Season stats of every defenseman and forward of one season stage.

## Usage

``` r
vhl_skater_stats(season, stage = "regular", return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With
  `stage` it is mapped to the site's tournament id, see
  [`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md).

- stage:

  Season stage: `"regular"` (default) or `"playoff"`.

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per skater and team:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
| `position_group` | character | Position group: defense or forward. |
| `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
| `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
| `jersey` | integer | Jersey number (#; the page legend mislabels this column 'Position'). |
| `team` | character | Team short name (Latin; the report tables' form). |
| `games_played` | integer | Games played (GP). |
| `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `points` | integer | Points, goals + assists (PTS). |
| `plus_minus` | integer | Plus/minus (+/-). |
| `plus` | integer | Plus: on-ice goals for that count toward plus/minus (+). |
| `minus` | integer | Minus: on-ice goals against that count toward plus/minus (-). |
| `penalty_minutes` | integer | Penalty minutes (PIM). |
| `even_strength_goals` | integer | Even-strength goals (ESG). |
| `power_play_goals` | integer | Power-play goals (PPG). |
| `shorthanded_goals` | integer | Shorthanded goals (SHG). |
| `overtime_goals` | integer | Overtime goals (OTG). |
| `game_winning_goals` | integer | Game-winning goals (GWG). |
| `shootout_deciding_goals` | integer | Shootout-deciding goals (SDS; the legend says 'Shootouts deciding shots'). |
| `shots_on_goal` | integer | Shots on goal (SOG). |
| `shot_pct` | numeric | Shooting percentage, goals / shots on goal x 100 (%SOG); 0-100 scale; null when the page shows '-'. |
| `shots_per_game` | numeric | Shots on goal per game (S/G). |
| `faceoffs` | integer | Faceoffs taken (FO). |
| `faceoffs_won` | integer | Faceoffs won (FOW). |
| `faceoff_pct` | numeric | Faceoff win percentage (%FO); 0-100 scale; null when the page shows '-' (no faceoffs taken). |
| `toi_per_game` | character | Average time on ice per game (TOI/G), 'm:ss' / 'mm:ss' string; null when the page shows '-'. |
| `shifts_per_game` | numeric | Average shifts per game (Sft/G); integer in a one-game report, decimal in season tables. |
| `hits` | integer | Body checks delivered (Hits). |
| `blocked_shots` | integer | Blocked shots (BLS). |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Source: `GET https://www.vhlru.ru/en/stats/players/{season}/`
(server-rendered HTML, keyless; parsed with rvest). Unofficial use of a
public source; payloads are not redistributed.

## Owner flag (sanctions)

The VHL is run by Russian league operators, and several clubs are owned
or sponsored by sanctioned state companies. Fetching on your own machine
is fine; a published data release or a hosted mirror of this data needs
the owner's sanctions / legal sign-off first.

## Errors

- `fastRhockey_no_data`: the VHL has nothing for the request (a season
  without that stage, a game not in the season's calendar, an unknown
  game, player or team page).

- `fastRhockey_bad_request`: a bad `season` / `stage` / id, a missing
  argument, or HTTP 400 / 422.

- `fastRhockey_asset_fetch`: the page could not be fetched, or it is not
  the page asked for (another season, game or player; a missing table or
  header drift).

## See also

Other VHL Functions:
[`vhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/vhl_game_info.md),
[`vhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_box.md),
[`vhl_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_stats.md),
[`vhl_goals()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goals.md),
[`vhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/vhl_pbp.md),
[`vhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/vhl_penalties.md),
[`vhl_player()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player.md),
[`vhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_box.md),
[`vhl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_career.md),
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_skater_stats(season = 2026))
#> ── VHL Skater Stats Information from vhlru.ru ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:21:29 UTC
#> # A tibble: 1,140 × 32
#>    season stage     tn position_group player_id player jersey team  games_played
#>     <int> <chr>  <int> <chr>              <int> <chr>   <int> <chr>        <int>
#>  1   2026 regul…  1374 defense            22129 Miney…     51 Meta…           49
#>  2   2026 regul…  1374 defense            15836 Berdy…     38 Omsk…           55
#>  3   2026 regul…  1374 defense            24805 Trish…     17 Ryaz…           55
#>  4   2026 regul…  1374 defense            22248 Popov…     11 Rubin           43
#>  5   2026 regul…  1374 defense            34892 Budni…     85 Toros           59
#>  6   2026 regul…  1374 defense            20642 Naumo…      8 Meta…           60
#>  7   2026 regul…  1374 defense            30974 Stikh…     55 Rubin           57
#>  8   2026 regul…  1374 defense            35793 Boldy…     83 Kris…           57
#>  9   2026 regul…  1374 defense            13985 Ivano…      3 Khim…           36
#> 10   2026 regul…  1374 defense            25729 Stari…     75 Meta…           54
#> # ℹ 1,130 more rows
#> # ℹ 23 more variables: goals <int>, assists <int>, points <int>,
#> #   plus_minus <int>, plus <int>, minus <int>, penalty_minutes <int>,
#> #   even_strength_goals <int>, power_play_goals <int>, shorthanded_goals <int>,
#> #   overtime_goals <int>, game_winning_goals <int>,
#> #   shootout_deciding_goals <int>, shots_on_goal <int>, shot_pct <dbl>,
#> #   shots_per_game <dbl>, faceoffs <int>, faceoffs_won <int>, …
# }
```
