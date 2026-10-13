# VHL Goals

Goals of a game with scorer, assists, strength and the jerseys on ice
for both teams.

## Usage

``` r
vhl_goals(season, game_id, stage = "regular", return_parsed = TRUE)
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

A `fastRhockey_data` tibble with one row per goal:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `game_number` | integer | Official game number (from 'No.984'); checked against the request. |
| `team_side` | character | Side of the game the row belongs to: home or away. |
| `team_page_id` | integer | Team page id (vhlru.ru `/en/teams/{team_page_id}/`), stable across seasons; for report rows the page id of the row's side. |
| `goal_number` | integer | Goal number in the game (G). |
| `period` | character | Period (Per) as printed: '1', '2', '3', overtime unverified, 'RB' (Cyrillic even on /en/: shootout, reshayushchiy brosok). |
| `time` | character | Elapsed game time 'mm:ss' from the opening faceoff (Time); the shootout row says '65:00'. |
| `score` | character | Score after the goal 'home:away' (Score). |
| `strength` | character | Goal strength code (PMG): eq (even), ppg (power play), shg (shorthanded); null on the shootout row. |
| `scorer_id` | integer | Player id of the scorer: the `{player_id}` of `/en/players/{player_id}/`, stable across seasons and teams; also the photo id on img.khl.ru. Same numeric sequence as khl.ru player ids; whether it equals the same person's khl.ru id is unverified. |
| `scorer` | character | Goal scorer's name, 'Surname Given' as the report prints it. |
| `scorer_jersey` | integer | Scorer jersey number. |
| `scorer_season_total` | integer | The scorer's season goal count after this goal (the '(13)' suffix); null on the shootout row. |
| `assist1_id` | integer | Player id of the first assist; null when unassisted. |
| `assist1` | character | First assist name; null when unassisted. |
| `assist1_jersey` | integer | First assist jersey number. |
| `assist1_season_total` | integer | The first assist's season assist count after this goal (the '(14)' suffix). |
| `assist2_id` | integer | Player id of the second assist; null when none. |
| `assist2` | character | Second assist name; null when none. |
| `assist2_jersey` | integer | Second assist jersey number. |
| `assist2_season_total` | integer | The second assist's season assist count after this goal. |
| `home_on_ice` | list of integer | Jersey numbers of the home players on ice at the goal (goalie included); the column header is the home team's short name. Shootout row: the goalie only. |
| `away_on_ice` | list of integer | Jersey numbers of the away players on ice at the goal (goalie included); the column header is the away team's short name. Empty on the shootout row. |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

One report page feeds
[`vhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/vhl_game_info.md),
`vhl_goals()`,
[`vhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/vhl_penalties.md),
[`vhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_box.md)
and
[`vhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_box.md).
Each call fetches it (and, for the first game of a season, that season's
calendar, cached for the R session). Use `return_parsed = FALSE` to keep
the page's record.

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
  try(vhl_goals(season = 2026, game_id = 899223))
#> ── VHL Goals Information from vhlru.ru ─────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 02:00:21 UTC
#> # A tibble: 5 × 25
#>   game_id season stage   game_number team_side team_page_id goal_number period
#>     <int>  <int> <chr>         <int> <chr>            <int>       <int> <chr> 
#> 1  899223   2026 regular         984 away            505612           1 1     
#> 2  899223   2026 regular         984 away            505612           2 1     
#> 3  899223   2026 regular         984 home               390           3 3     
#> 4  899223   2026 regular         984 home               390           4 3     
#> 5  899223   2026 regular         984 away            505612           5 РБ    
#> # ℹ 17 more variables: time <chr>, score <chr>, strength <chr>,
#> #   scorer_id <int>, scorer <chr>, scorer_jersey <int>,
#> #   scorer_season_total <int>, assist1_id <int>, assist1 <chr>,
#> #   assist1_jersey <int>, assist1_season_total <int>, assist2_id <int>,
#> #   assist2 <chr>, assist2_jersey <int>, assist2_season_total <int>,
#> #   home_on_ice <list>, away_on_ice <list>
# }
```
