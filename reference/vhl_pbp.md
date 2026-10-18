# VHL Play-by-Play (text broadcast)

Text broadcast of a game (online.vhlru.ru, Russian text): goals,
penalties, shootout attempts, goalie changes and comments. No shot
coordinates.

## Usage

``` r
vhl_pbp(game_id, season, stage = "regular", return_parsed = TRUE)
```

## Arguments

- game_id:

  Game id (game_id in vhl_schedule; the report's and the broadcast's
  idgame), e.g. 899223.

- season:

  Required. Season END year of the game (2026 = 2025/26); with `stage`
  it locates the game in the calendar, whose game number the broadcast
  page must carry (the page names no game id).

- stage:

  Season stage: `"regular"` (default) or `"playoff"`.

- return_parsed:

  `TRUE` (default) returns the parsed tibble; `FALSE` returns the page's
  parsed record (a named list, the sdv-internal-refs capture shape, plus
  `_request` with the season, stage, tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per event:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `game_number` | integer | Official game number ('Igra nomer 984'); checked against the request. |
| `event_index` | integer | Order of the event on the page, 0 = the first listed (the broadcast lists the latest event first). |
| `kind` | character | Event kind from the CSS modifier: game_state (ga), comment (co), goal (go), penalty (pn), goalie_change (gc), video_review (vr), shootout_attempt (bu). |
| `kind_code` | character | Raw CSS modifier of .game-event-item–`code`. |
| `time` | character | Time label verbatim: 'mm:ss' elapsed game time, 'NN min' (minute of a comment), or 'HH:MM' wall clock on game_state events; null when absent. See clock. |
| `clock` | character | What time means: game (elapsed mm:ss), minute ('58 min'), wall (local wall clock, game_state rows); null when there is no time. |
| `score` | character | Score 'home-away' (dash, not colon) after a goal; on a shootout attempt, the shootout score printed beside it (before or after that attempt: unverified). |
| `strength_code` | character | Goal strength code in Cyrillic: RAV (even), BOL (power play), MEN (shorthanded, unobserved), BUL (penalty shot / shootout); empty string seen as null. |
| `strength` | character | strength_code in English: even, power_play, shorthanded, penalty_shot_or_shootout. |
| `text` | character | Event text verbatim in RUSSIAN (team, players, infraction, period stats); not translated. |
| `player_ids` | list of integer | Player ids linked in the event text (`//vhlru.ru/players/{id}/`), scorer first; empty for most kinds. |
| `home_team` | character | Home team name in Cyrillic. |
| `away_team` | character | Away team name in Cyrillic. |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

The broadcast has no `/en/` mirror: event text, team names and strength
codes are Russian. `season` (and `stage`) locate the game in the
calendar, whose game number the page must carry (the page names no game
id).

Source: `GET https://online.vhlru.ru/online/{game_id}.html`
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
[`vhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/vhl_penalties.md),
[`vhl_player()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player.md),
[`vhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_box.md),
[`vhl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_career.md),
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/vhl_schedule.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_pbp(game_id = 899223, season = 2026))
#> ── VHL Play-by-Play Information from vhlru.ru ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:19:08 UTC
#> # A tibble: 61 × 16
#>    game_id season stage   game_number event_index kind     kind_code time  clock
#>      <int>  <int> <chr>         <int>       <int> <chr>    <chr>     <chr> <chr>
#>  1  899223   2026 regular         984           0 game_st… ga        21:20 wall 
#>  2  899223   2026 regular         984           1 goal     go        65:00 game 
#>  3  899223   2026 regular         984           2 game_st… ga        21:20 wall 
#>  4  899223   2026 regular         984           3 shootou… bu        NA    NA   
#>  5  899223   2026 regular         984           4 shootou… bu        NA    NA   
#>  6  899223   2026 regular         984           5 shootou… bu        NA    NA   
#>  7  899223   2026 regular         984           6 shootou… bu        NA    NA   
#>  8  899223   2026 regular         984           7 shootou… bu        NA    NA   
#>  9  899223   2026 regular         984           8 shootou… bu        NA    NA   
#> 10  899223   2026 regular         984           9 shootou… bu        NA    NA   
#> # ℹ 51 more rows
#> # ℹ 7 more variables: score <chr>, strength_code <chr>, strength <chr>,
#> #   text <chr>, player_ids <list>, home_team <chr>, away_team <chr>
# }
```
