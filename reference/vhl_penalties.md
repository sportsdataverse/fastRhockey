# VHL Penalties

Penalties of a game: team, time, player, minutes and infraction.

## Usage

``` r
vhl_penalties(season, game_id, stage = "regular", return_parsed = TRUE)
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

A `fastRhockey_data` tibble with one row per penalty:

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
| `time` | character | Elapsed game time of the penalty 'mm:ss'. |
| `player_id` | integer | Player id of the penalised player; null for a bench (team) penalty. |
| `player` | character | Penalised player name; null for a bench (team) penalty. |
| `player_jersey` | integer | Penalised player's jersey number. |
| `minutes` | integer | Penalty length in minutes (the unlabelled third column). |
| `infraction` | character | Infraction in English ('hooking'); null when the site leaves it blank. |

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
  try(vhl_penalties(season = 2026, game_id = 899223))
#> ── VHL Penalties Information from vhlru.ru ─────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:52:38 UTC
#> # A tibble: 3 × 13
#>   game_id season stage  game_number team_side team_page_id team  time  player_id
#>     <int>  <int> <chr>        <int> <chr>            <int> <chr> <chr>     <int>
#> 1  899223   2026 regul…         984 home               390 Toros 45:16     25292
#> 2  899223   2026 regul…         984 away            505612 Magn… 39:17     23837
#> 3  899223   2026 regul…         984 away            505612 Magn… 42:08     41489
#> # ℹ 4 more variables: player <chr>, player_jersey <int>, minutes <int>,
#> #   infraction <chr>
# }
```
