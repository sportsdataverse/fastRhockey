# **KHL Play-by-Play**

Play-by-play: shots, faceoffs, hits, blocks, giveaways, takeaways,
penalties and goals (2008-09 on).

## Usage

``` r
khl_pbp(
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

A `fastRhockey_data` tibble (26 columns; zero rows with the same columns
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
| event_index | integer | 0-based order of the event in the game, oldest first |
| event_type | character | period (start / end marker), faceoff, shot, block, action, penalty or goal |
| time | character | Game clock mm:ss (game-elapsed); period marker rows keep their wall-clock HH:MM |
| time_seconds | integer | Game-elapsed seconds (null on period marker rows) |
| team_side | character | A = home, B = visitor |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| result | character | Event result code (shots: goal / saved / miss; actions: hit, gva = giveaway, tka = takeaway, intc = interception) |
| result_text | character | Event result label |
| penalty_minutes | integer | Penalty minutes (penalty events) |
| score_a | integer | Home goals after the goal |
| score_b | integer | Visitor goals after the goal |
| won_side | character | Faceoff winner's side (A / B) |
| won_jn | integer | Faceoff winner's jersey |
| won_player_id | integer | Faceoff winner's khl.ru player id |
| lost_side | character | Faceoff loser's side (A / B) |
| lost_jn | integer | Faceoff loser's jersey |
| lost_player_id | integer | Faceoff loser's khl.ru player id |
| won_zone | character | Zone of the faceoff from the winner's view: offensive, neutral or defensive |
| shooter_side | character | Blocked shooter's side (A / B) |
| shooter_jn | integer | Blocked shooter's jersey |
| shooter_player_id | integer | Blocked shooter's khl.ru player id |
| period_action | character | Period marker: 0 = start, 1 = end (inferred) |

## Details

Endpoint: `POST https://www.khl.ru/rest/game/text/`.

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
[`khl_game_protocol()`](https://fastRhockey.sportsdataverse.org/reference/khl_game_protocol.md),
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
  try(khl_pbp(game_id = 898236, season = 2026))
#> ── KHL Play-by-Play from khl.ru ────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:08 UTC
#> # A tibble: 268 × 26
#>    game_id tournament_id period event_index event_type time  time_seconds
#>      <int>         <int>  <int>       <int> <chr>      <chr>        <int>
#>  1  898236          1369      1           0 period     19:31           NA
#>  2  898236          1369      1           1 faceoff    00:00            0
#>  3  898236          1369      1           2 action     00:04            4
#>  4  898236          1369      1           3 action     00:19           19
#>  5  898236          1369      1           4 action     00:38           38
#>  6  898236          1369      1           5 action     00:45           45
#>  7  898236          1369      1           6 action     01:54          114
#>  8  898236          1369      1           7 action     02:05          125
#>  9  898236          1369      1           8 action     02:05          125
#> 10  898236          1369      1           9 action     02:11          131
#> # ℹ 258 more rows
#> # ℹ 19 more variables: team_side <chr>, jn <int>, player_id <int>,
#> #   result <chr>, result_text <chr>, penalty_minutes <int>, score_a <int>,
#> #   score_b <int>, won_side <chr>, won_jn <int>, won_player_id <int>,
#> #   lost_side <chr>, lost_jn <int>, lost_player_id <int>, won_zone <chr>,
#> #   shooter_side <chr>, shooter_jn <int>, shooter_player_id <int>,
#> #   period_action <chr>
# }
```
