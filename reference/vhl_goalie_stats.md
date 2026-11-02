# VHL Goalie Stats

Season stats of every goalie of one season stage.

## Usage

``` r
vhl_goalie_stats(season, stage = "regular", return_parsed = TRUE)
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
  parsed record (a named list, plus `_request` with the season, stage,
  tn and the ids asked for).

## Value

A `fastRhockey_data` tibble with one row per goalie and team:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
| `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
| `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
| `jersey` | integer | Jersey number (#; the page legend mislabels this column 'Position'). |
| `team` | character | Team short name (Latin; the report tables' form). |
| `games_played` | integer | Games played (GP). |
| `wins` | integer | Goalie wins (W). |
| `losses` | integer | Goalie losses (L). |
| `shootouts_played` | integer | Shootouts played (SOP). |
| `shots_against` | integer | Shots on goal faced (the SOG column of goalie tables). |
| `goals_against` | integer | Goals allowed (GA). |
| `saves` | integer | Saves (Sv): shots on goal the goalie stopped in the row's scope (the game, or the season stage in stats and career rows). |
| `save_pct` | numeric | Save percentage (%Sv); 0-100 scale (93.8 = 93.8 %); null when the page shows '-'. |
| `gaa` | numeric | Goals-against average (GAA) = 60 x GA / TOI minutes; null when the page shows '-'. |
| `goals` | integer | Goals (G) the player scored in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `shutouts` | integer | Shutouts (SO): games in the row's scope in which the goalie allowed no goal. |
| `penalty_minutes` | integer | Penalty minutes (PIM). |
| `toi` | character | Time on ice (TOI), 'mm:ss' string; minutes run past 59 over a season ('2314:48'); null when the page shows '-'. |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Source: `GET https://www.vhlru.ru/en/stats/players/{season}/`
(server-rendered HTML, keyless; parsed with rvest). Unofficial use of a
public source; payloads are not redistributed.

## Terms of use

Check the provider's terms of use before redistributing this data.

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
[`vhl_goals()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goals.md),
[`vhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/vhl_pbp.md),
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
if (FALSE) { # interactive()
# \donttest{
  try(vhl_goalie_stats(season = 2026, stage = "playoff"))
# }
}
```
