# VHL Player Box Score

Skater box score of a game: scoring, shots, faceoffs, time on ice, hits
and blocks.

## Usage

``` r
vhl_player_box(season, game_id, stage = "regular", return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (2026 = 2025/26; the calendar covers 2010/11 on). With
  `stage` it is mapped to the site's tournament id, see
  [`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md).

- game_id:

  Required. Game id (game_id in vhl_schedule; the report's and the
  broadcast's idgame), e.g. 899223.

- stage:

  Season stage: `"regular"` (default) or `"playoff"`.

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per skater:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `game_number` | integer | Official game number (from 'No.984'); checked against the request. |
| `team_side` | character | Side of the game the row belongs to: home or away. |
| `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
| `team` | character | Team short name (Latin; the report tables' form). |
| `position_group` | character | Position group: defense or forward. |
| `jersey` | integer | Jersey number (integer). |
| `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
| `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
| `captaincy` | character | Captaincy marker from the name suffix: C (captain), A (alternate); null otherwise. |
| `games_played` | integer | Games played (GP). |
| `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `points` | integer | Points, goals + assists (PTS). |
| `plus_minus` | integer | Plus/minus (+/-). |
| `penalty_minutes` | integer | Penalty minutes (PIM). |
| `even_strength_goals` | integer | Even-strength goals (ESG). |
| `power_play_goals` | integer | Power-play goals (PPG). |
| `shorthanded_goals` | integer | Shorthanded goals (SHG). |
| `overtime_goals` | integer | Overtime goals (OTG). |
| `game_winning_goals` | integer | Game-winning goals (GWG). |
| `shootout_deciding_goals` | integer | Shootout-deciding goals (SDS; the legend says 'Shootouts deciding shots'). |
| `shots_on_goal` | integer | Shots on goal (SOG). |
| `shot_pct` | numeric | Shooting percentage, goals / shots on goal x 100 (%SOG); 0-100 scale; null when the page shows '-'. |
| `faceoffs` | integer | Faceoffs taken (FO). |
| `faceoffs_won` | integer | Faceoffs won (FOW). |
| `faceoff_pct` | numeric | Faceoff win percentage (%FO); 0-100 scale; null when the page shows '-' (no faceoffs taken). |
| `toi_per_game` | character | Time on ice in this game (TOI/G with GP 1), 'mm:ss'. |
| `shifts_per_game` | numeric | Shifts in this game (Sft/G with GP 1). |
| `hits` | integer | Body checks delivered (Hits). |
| `blocked_shots` | integer | Blocked shots (BLS). |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Source: `GET https://www.vhlru.ru/en/report/{season}/` (server-rendered
HTML, keyless; parsed with rvest). Unofficial use of a public source;
payloads are not redistributed.

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
[`vhl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_career.md),
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_player_box(season = 2026, game_id = 899223))
#> ── VHL Player Box Information from vhlru.ru ────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:12:44 UTC
#> # A tibble: 34 × 33
#>    game_id season stage  game_number team_side team_page_id team  position_group
#>      <int>  <int> <chr>        <int> <chr>            <int> <chr> <chr>         
#>  1  899223   2026 regul…         984 home               390 Toros defense       
#>  2  899223   2026 regul…         984 home               390 Toros defense       
#>  3  899223   2026 regul…         984 home               390 Toros defense       
#>  4  899223   2026 regul…         984 home               390 Toros defense       
#>  5  899223   2026 regul…         984 home               390 Toros defense       
#>  6  899223   2026 regul…         984 home               390 Toros defense       
#>  7  899223   2026 regul…         984 home               390 Toros defense       
#>  8  899223   2026 regul…         984 home               390 Toros forward       
#>  9  899223   2026 regul…         984 home               390 Toros forward       
#> 10  899223   2026 regul…         984 home               390 Toros forward       
#> # ℹ 24 more rows
#> # ℹ 25 more variables: jersey <int>, player_id <int>, player <chr>,
#> #   captaincy <chr>, games_played <int>, goals <int>, assists <int>,
#> #   points <int>, plus_minus <int>, penalty_minutes <int>,
#> #   even_strength_goals <int>, power_play_goals <int>, shorthanded_goals <int>,
#> #   overtime_goals <int>, game_winning_goals <int>,
#> #   shootout_deciding_goals <int>, shots_on_goal <int>, shot_pct <dbl>, …
# }
```
