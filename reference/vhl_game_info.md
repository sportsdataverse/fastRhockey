# VHL Game Info

Game header of a report: date, attendance, score by period, teams,
coaches and officials.

## Usage

``` r
vhl_game_info(season, game_id, stage = "regular", return_parsed = TRUE)
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

A `fastRhockey_data` tibble with one row per game (one row):

|  |  |  |
|----|----|----|
| col_name | type | description |
| `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `game_number` | integer | Official game number (from 'No.984'); checked against the request. |
| `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
| `date` | character | Game date, ISO 'YYYY-MM-DD', parsed from date_text. |
| `local_time` | character | Start time 'HH:MM' from date_text; local time zone unverified (likely Moscow). |
| `date_text` | character | Date line verbatim; RUSSIAN month and weekday even on /en/ ('18 Mart 2026, Sreda, 17:00'). The `time datetime` attribute is a constant placeholder (2020-01-23 18:00) and is ignored. |
| `attendance` | integer | Attendance (from '910 sp'). |
| `score` | character | Final score 'home:away' (the shootout counts as one goal). |
| `home_score` | integer | Home final goals. |
| `away_score` | integer | Away final goals. |
| `decision` | character | OT or SO when the game went past regulation; null otherwise. |
| `period_scores` | list of character | Score of each period 'home:away', overtime and shootout included (SO game: '0:2','0:0','2:0','0:0','0:1'). |
| `home_team_page_id` | integer | Home team page id. Team page id (vhlru.ru only): the `{team_id}` of `/en/teams/{team_id}/` (Toros = 390). Neither a club id nor a team-season id; it joins nothing else (bridge: the calendar row of the same game_id). |
| `home_team` | character | Home team name (Latin, FULL form: 'Kristall S'; the report's tables use the short 'Kristall'). |
| `home_short` | character | Home team short name (the report tables' form). |
| `home_city` | character | Home team city. |
| `home_coach` | character | Home head coach, 'Surname Given'. |
| `away_team_page_id` | integer | Away team page id. Team page id (vhlru.ru only): the `{team_id}` of `/en/teams/{team_id}/` (Toros = 390). Neither a club id nor a team-season id; it joins nothing else (bridge: the calendar row of the same game_id). |
| `away_team` | character | Away team name (Latin, FULL form). |
| `away_short` | character | Away team short name (the report tables' form). |
| `away_city` | character | Away team city. |
| `away_coach` | character | Away head coach, 'Surname Given'. |
| `referee1_number` | integer | First referee's sweater number. |
| `referee1` | character | First referee, 'Surname Given'. |
| `referee2_number` | integer | Second referee's sweater number. |
| `referee2` | character | Second referee, 'Surname Given'. |
| `linesman1_number` | integer | First linesman's sweater number. |
| `linesman1` | character | First linesman, 'Surname Given'. |
| `linesman2_number` | integer | Second linesman's sweater number. |
| `linesman2` | character | Second linesman, 'Surname Given'. |

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
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
# \donttest{
  try(vhl_game_info(season = 2026, game_id = 899223))
#> ── VHL Game Info Information from vhlru.ru ─────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:21:06 UTC
#> # A tibble: 1 × 32
#>   game_id season stage   game_number    tn date  local_time date_text attendance
#>     <int>  <int> <chr>         <int> <int> <chr> <chr>      <chr>          <int>
#> 1  899223   2026 regular         984  1374 2026… 17:00      18 Март …        910
#> # ℹ 23 more variables: score <chr>, home_score <int>, away_score <int>,
#> #   decision <chr>, period_scores <list>, home_team_page_id <int>,
#> #   home_team <chr>, home_short <chr>, home_city <chr>, home_coach <chr>,
#> #   away_team_page_id <int>, away_team <chr>, away_short <chr>,
#> #   away_city <chr>, away_coach <chr>, referee1_number <int>, referee1 <chr>,
#> #   referee2_number <int>, referee2 <chr>, linesman1_number <int>,
#> #   linesman1 <chr>, linesman2_number <int>, linesman2 <chr>
# }
```
