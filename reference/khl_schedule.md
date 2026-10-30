# **KHL Schedule**

Season schedule and results of a tournament, one row per game (All-Star
games dropped).

## Usage

``` r
khl_schedule(
  season = NULL,
  stage = "regular",
  headers = NULL,
  tournament_id = NULL,
  return_parsed = TRUE
)
```

## Arguments

- season:

  Season by its END year (2026 = 2025-26), mapped to the KHL tournament
  id (tnId) through the webcaster season table (khl_webcaster_stages).
  NULL = the site's current season.

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

A `fastRhockey_data` tibble (16 columns; zero rows with the same columns
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
| date | character | Game date YYYY-MM-DD (Moscow time) |
| time | character | Start time hh:mm (Moscow time); not a game clock |
| home_club_id | integer | Home CLUB id (stable across seasons; joins webcaster team khl_id) |
| home_name | character | Home team name (Russian on www.khl.ru) |
| away_club_id | integer | Visitor CLUB id (stable across seasons; joins webcaster team khl_id) |
| away_name | character | Visitor team name (Russian on www.khl.ru) |
| home_score | integer | Home goals (final, shootout winner counted) |
| away_score | integer | Visitor goals (final, shootout winner counted) |
| decision | character | OT or SO when decided in overtime / a shootout; null in regulation |
| period_scores | character | Per-period home:away scores, comma-joined |
| winner_club_id | integer | Club id of the winner |
| approved | integer | 1 = result confirmed by the league |
| arena_id | integer | khl.ru arena id |
| arena_name | character | Arena (city) name, from data.ARENAS |

## Details

Endpoint: `POST https://www.khl.ru/rest/calendar/list/`.

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
  try(khl_schedule(season = 2026))
#> ── KHL Schedule from khl.ru ────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:03 UTC
#> # A tibble: 748 × 16
#>    game_id tournament_id date       time  home_club_id home_name    away_club_id
#>      <int>         <int> <chr>      <chr>        <int> <chr>               <int>
#>  1  897491          1369 2025-09-05 19:30            1 Локомотив              25
#>  2  897492          1369 2025-09-06 14:30           37 Металлург Мг           53
#>  3  897494          1369 2025-09-06 16:00            2 ЦСКА                  719
#>  4  897493          1369 2025-09-06 17:00           26 Торпедо                38
#>  5  897495          1369 2025-09-06 17:00          568 Драконы                24
#>  6  897499          1369 2025-09-07 13:30           29 Сибирь                 54
#>  7  897496          1369 2025-09-07 14:30          198 Барыс                  25
#>  8  897497          1369 2025-09-07 17:00            7 Спартак               451
#>  9  897498          1369 2025-09-07 17:00           71 Нефтехимик             56
#> 10  897501          1369 2025-09-08 16:30           34 Авангард               53
#> # ℹ 738 more rows
#> # ℹ 9 more variables: away_name <chr>, home_score <int>, away_score <int>,
#> #   decision <chr>, period_scores <chr>, winner_club_id <int>, approved <int>,
#> #   arena_id <int>, arena_name <chr>
# }
```
