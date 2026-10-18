# **KHL Game Protocol**

Game protocol: one row per goal with scorer, assistants, strength and
the jerseys on the ice.

## Usage

``` r
khl_game_protocol(
  game_id,
  season = NULL,
  stage = "regular",
  headers = NULL,
  tournament_id = NULL,
  return_parsed = TRUE
)
```

## Arguments

- game_id:

  Required. khl.ru game id (the schedule's game_id; equals webcaster
  event khl_id).

- season:

  Season by its END year (2026 = 2025-26), mapped to the KHL tournament
  id (tnId) through the webcaster season table (khl_webcaster_stages).
  The game's season; required unless tournament_id is given.

- stage:

  Which stage of the season: regular or playoff. One of `regular`,
  `playoff`.

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- tournament_id:

  khl.ru tournament id (tnId), sent as is instead of season / stage
  (1369 = 2025-26 regular season, 1370 its playoff; the schedule's
  tournament_id column). The All-Star games have their own.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

## Value

A `fastRhockey_data` tibble (21 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | integer | khl.ru game id; equals webcaster event khl_id / match_id |
| tournament_id | integer | khl.ru tournament (stage) id, tnId: 1369 = 2025-26 regular season, 1370 its playoff; equals webcaster stage khl_id |
| period | integer | Period number (box score: 0 = the whole game; 4+ = overtime) |
| goal_number | integer | Goal number in the game |
| time | character | Game clock mm:ss (game-elapsed); period marker rows keep their wall-clock HH:MM |
| team_side | character | A = home, B = visitor |
| score_a | integer | Home goals after the goal |
| score_b | integer | Visitor goals after the goal |
| strength | character | Strength code as the site prints it (Russian: RAV even, BOL power play, MEN short-handed) |
| strength_text | character | Strength label as the site prints it |
| scorer_player_id | integer | Scorer's khl.ru player id |
| scorer_name | character | Scorer's name with the season count in brackets |
| scorer_jn | integer | Scorer's jersey |
| assist_1_player_id | integer | First assistant's khl.ru player id |
| assist_1_name | character | First assistant's name with the season count in brackets |
| assist_1_jn | integer | First assistant's jersey |
| assist_2_player_id | integer | Second assistant's khl.ru player id |
| assist_2_name | character | Second assistant's name with the season count in brackets |
| assist_2_jn | integer | Second assistant's jersey |
| on_ice_a | character | Home jerseys on the ice, comma-separated |
| on_ice_b | character | Visitor jerseys on the ice, comma-separated |

## Details

Endpoint: `POST https://www.khl.ru/rest/game/protocol/`.

khl.ru REST needs a primed session (a GET of the site root sets the
cookie jar and the Bitrix `sessid`, then each call POSTs a form); this
is done for you, one session per host per R session, re-primed when the
site rotates its cookie or the `sessid` expires. No route takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
tournament id (tnId) through the webcaster season table, or pass
`tournament_id` (the All-Star games have their own tnId).

Ids and counts are integers (sdv-py Int64); a column holding values past
2^31 (epoch milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

**Owner flag (sanctions).** The KHL and its webcaster platform are run
by Russian operators, some clubs by sanctioned state companies. Fetching
on your own machine is fine; any published data release, hosted mirror
or redistribution needs owner (sanctions / legal) sign-off first.

## See also

Other KHL Functions:
[`khl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_header.md),
[`khl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_info.md),
[`khl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/khl_pbp.md),
[`khl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_box.md),
[`khl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_career.md),
[`khl_player_profile()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_profile.md),
[`khl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_stats.md),
[`khl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/khl_playoff_bracket.md),
[`khl_roster()`](https://fastRhockey.sportsdataverse.org/reference/khl_roster.md),
[`khl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/khl_schedule.md),
[`khl_shots()`](https://fastRhockey.sportsdataverse.org/reference/khl_shots.md),
[`khl_standings()`](https://fastRhockey.sportsdataverse.org/reference/khl_standings.md),
[`khl_stat_filters()`](https://fastRhockey.sportsdataverse.org/reference/khl_stat_filters.md),
[`khl_webcaster_event()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_event.md),
[`khl_webcaster_events()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_events.md),
[`khl_webcaster_leaders()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_leaders.md),
[`khl_webcaster_players()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players.md),
[`khl_webcaster_players_light()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_players_light.md),
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md),
[`khl_webcaster_tables()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_tables.md),
[`khl_webcaster_team()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_team.md),
[`khl_webcaster_teams()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_teams.md)

## Examples

``` r
# \donttest{
  try(khl_game_protocol(game_id = 898236, season = 2026))
#> ── KHL Game Protocol from khl.ru ───────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:07 UTC
#> # A tibble: 5 × 21
#>   game_id tournament_id period goal_number time  team_side score_a score_b
#>     <int>         <int>  <int>       <int> <chr> <chr>       <int>   <int>
#> 1  898236          1369      2           1 22:02 B               0       1
#> 2  898236          1369      2           2 23:41 A               1       1
#> 3  898236          1369      2           3 34:47 A               2       1
#> 4  898236          1369      2           4 35:08 B               2       2
#> 5  898236          1369      4           5 65:00 B               2       3
#> # ℹ 13 more variables: strength <chr>, strength_text <chr>,
#> #   scorer_player_id <int>, scorer_name <chr>, scorer_jn <int>,
#> #   assist_1_player_id <int>, assist_1_name <chr>, assist_1_jn <int>,
#> #   assist_2_player_id <int>, assist_2_name <chr>, assist_2_jn <int>,
#> #   on_ice_a <chr>, on_ice_b <chr>
# }
```
