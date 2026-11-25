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

Ids and counts are integers; a column holding values past 2^31 (epoch
milliseconds) is returned as a whole-number double.

At least one second is left between two requests to a host.

Check the provider's terms of use before redistributing this data.

## See also

[family_khl](https://fastRhockey.sportsdataverse.org/reference/family_khl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(khl_pbp(game_id = 898236, season = 2026))
#> Error in .khl_prime(session, timeout) : 
#>   khl.ru served an anti-bot challenge page; try again later or through
#> `getOption('fastRhockey.proxy')`.
# }
```
