# **KHL Shot Map**

Shot map: one row per shot with its x / y on the 1000 x 500 rink grid
(2024-25 on).

## Usage

``` r
khl_shots(
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

A `fastRhockey_data` tibble (15 columns; zero rows with the same columns
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
| time | character | Game clock mm:ss (game-elapsed); period marker rows keep their wall-clock HH:MM |
| time_seconds | integer | Game-elapsed seconds (null on period marker rows) |
| team_side | character | A = home, B = visitor |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| shot_type | integer | 1 = goal, 2 = saved (inferred upstream; unverified) |
| x | integer | Shot x on the site's 1000-wide rink grid (indent_left = x/10 %); whether shots are normalised to one attacking direction is UNVERIFIED |
| y | integer | Shot y on the site's 500-high rink grid (indent_top = y/5 %); orientation UNVERIFIED |
| indent_left | numeric | Shot position as percent from the left edge |
| indent_top | numeric | Shot position as percent from the top edge |
| indent_right | numeric | Shot position as percent from the right edge |
| indent_bottom | numeric | Shot position as percent from the bottom edge |

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
[`khl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/khl_pbp.md),
[`khl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_box.md),
[`khl_player_career()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_career.md),
[`khl_player_profile()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_profile.md),
[`khl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/khl_player_stats.md),
[`khl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/khl_playoff_bracket.md),
[`khl_roster()`](https://fastRhockey.sportsdataverse.org/reference/khl_roster.md),
[`khl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/khl_schedule.md),
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
  try(khl_shots(game_id = 898236, season = 2026))
#> ── KHL Shot Map from khl.ru ────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:54:59 UTC
#> # A tibble: 69 × 15
#>    game_id tournament_id period time  time_seconds team_side    jn player_id
#>      <int>         <int>  <int> <chr>        <int> <chr>     <int>     <int>
#>  1  898236          1369      1 02:23          143 A            94     17624
#>  2  898236          1369      1 02:49          169 A            94     17624
#>  3  898236          1369      1 04:16          256 B            65     35032
#>  4  898236          1369      1 05:11          311 B            72     19216
#>  5  898236          1369      1 05:17          317 A            87     29928
#>  6  898236          1369      1 05:25          325 B            29     27904
#>  7  898236          1369      1 06:09          369 B             5     32390
#>  8  898236          1369      1 07:32          452 B            61     40421
#>  9  898236          1369      1 08:44          524 B            65     35032
#> 10  898236          1369      1 11:01          661 A            16     40673
#> # ℹ 59 more rows
#> # ℹ 7 more variables: shot_type <int>, x <int>, y <int>, indent_left <dbl>,
#> #   indent_top <dbl>, indent_right <dbl>, indent_bottom <dbl>
# }
```
