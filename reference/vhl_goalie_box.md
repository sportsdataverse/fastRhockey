# VHL Goalie Box Score

Goalie box score of a game: shots against, saves, save percentage and
time on ice.

## Usage

``` r
vhl_goalie_box(season, game_id, stage = "regular", return_parsed = TRUE)
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

A `fastRhockey_data` tibble with one row per goalie:

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
| `jersey` | integer | Jersey number (integer). |
| `player_id` | integer | Player id: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
| `player` | character | Player name, 'Surname Given' (Latin on /en/ pages). |
| `captaincy` | character | Unobserved in samples (null in every capture); type and meaning unverified. |
| `games_played` | integer | Games played (GP). |
| `wins` | integer | Goalie wins (W). |
| `losses` | integer | Goalie losses (L). |
| `shootouts_played` | integer | Shootouts played (SOP). |
| `shots_against` | integer | Shots on goal faced (the SOG column of goalie tables). |
| `goals_against` | integer | Goals allowed (GA). |
| `saves` | integer | Saves (Sv): shots on goal the goalie stopped in the row's scope (the game, or the season stage in stats and career rows). |
| `save_pct` | numeric | Save percentage (%Sv); 0-100 scale (93.8 = 93.8 %); null when the page shows '-'. |
| `gaa` | numeric | Goals-against average (GAA) = 60 x GA / TOI minutes; null when the page shows '-'. |
| `assists` | integer | Assists (A) credited to the player in the row's scope: the game in a box score, the season stage in stats and career rows. |
| `shutouts` | integer | Shutouts (SO): games in the row's scope in which the goalie allowed no goal. |
| `penalty_minutes` | integer | Penalty minutes (PIM). |
| `toi` | character | Time on ice in this game, 'mm:ss'; null for a dressed goalie who did not play ('-'). |

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
[`vhl_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_stats.md),
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
# \donttest{
  try(vhl_goalie_box(season = 2026, game_id = 899223))
#> ── VHL Goalie Box Information from vhlru.ru ────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:21:07 UTC
#> # A tibble: 4 × 24
#>   game_id season stage game_number team_side team_page_id team  jersey player_id
#>     <int>  <int> <chr>       <int> <chr>            <int> <chr>  <int>     <int>
#> 1  899223   2026 regu…         984 home               390 Toros     49     26207
#> 2  899223   2026 regu…         984 home               390 Toros      1     41850
#> 3  899223   2026 regu…         984 away            505612 Magn…     23     39266
#> 4  899223   2026 regu…         984 away            505612 Magn…     35     32042
#> # ℹ 15 more variables: player <chr>, captaincy <chr>, games_played <int>,
#> #   wins <int>, losses <int>, shootouts_played <int>, shots_against <int>,
#> #   goals_against <int>, saves <int>, save_pct <dbl>, gaa <dbl>, assists <int>,
#> #   shutouts <int>, penalty_minutes <int>, toi <chr>
# }
```
