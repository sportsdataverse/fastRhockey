# VHL Schedule

Schedule and results of one season stage: every game with its club ids,
score and decision.

## Usage

``` r
vhl_schedule(season, stage = "regular", return_parsed = TRUE)
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

A `fastRhockey_data` tibble with one row per game:

|  |  |  |
|----|----|----|
| col_name | type | description |
| `season` | integer | Season END year (2026 = 2025/26). |
| `stage` | character | Season stage: regular or playoff. |
| `tn` | integer | Tournament id of the season stage (2025/26 regular season = 1374, playoffs = 1375); vhl_tn maps season + stage to it. |
| `game_id` | integer | Game id (the calendar's game_id; the report's and the broadcast's idgame). The report and broadcast pages do not print it: it comes from the request. |
| `game_number` | integer | Official game number within the tournament (from 'No.986'). |
| `game_date` | character | Game date, ISO 'YYYY-MM-DD': the calendar day heading plus the season (August-December = START year). |
| `date` | character | Day header in English, day and month only ('18 March'); the page prints no year and no start time. |
| `weekday` | character | Weekday of the day header (English). |
| `home_team` | character | Home team name (Latin, FULL form: 'Kristall S'; standings, stats and the team page use the short 'Kristall'). |
| `home_club_id` | integer | Home club id: stable across seasons (the img.khl.ru logo path id). Neither a team-season id nor a team page id. |
| `away_team` | character | Away team name (Latin, FULL form, like home_team). |
| `away_club_id` | integer | Away club id: stable across seasons (the img.khl.ru logo path id). |
| `city` | character | City of the home team's arena (English). |
| `score` | character | Final score 'home:away'; '-:-' before the game. |
| `home_score` | integer | Home goals including the shootout winner goal; null before the game. |
| `away_score` | integer | Away goals including the shootout winner goal; null before the game. |
| `decision` | character | How the game ended beyond regulation: OT (overtime) or SO (shootout); null for regulation or unplayed games. |
| `winner` | character | Winning side from the win-home / win-visitor class: home or away; null before the game. |
| `status` | character | Unobserved in samples (null in every capture); type and meaning unverified. |
| `online_url` | character | Text-broadcast URL (`online.vhlru.ru/online/{game_id}.html`). |

With `return_parsed = FALSE`, the page's parsed record (a named list)
instead.

## Details

Source: `GET https://www.vhlru.ru/en/calendar/{season}/0/`
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
[`vhl_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goalie_stats.md),
[`vhl_goals()`](https://fastRhockey.sportsdataverse.org/reference/vhl_goals.md),
[`vhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/vhl_pbp.md),
[`vhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/vhl_penalties.md),
[`vhl_player()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player.md),
[`vhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_box.md),
[`vhl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/vhl_player_career.md),
[`vhl_roster()`](https://fastRhockey.sportsdataverse.org/reference/vhl_roster.md),
[`vhl_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/vhl_skater_stats.md),
[`vhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/vhl_standings.md),
[`vhl_tn()`](https://fastRhockey.sportsdataverse.org/reference/vhl_tn.md)

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
  try(vhl_schedule(season = 2026))
  try(vhl_schedule(season = 2026, stage = "playoff"))
# }
}
```
