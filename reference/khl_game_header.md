# **KHL Game Header**

Game header: status, teams, score, arena (one row).

## Usage

``` r
khl_game_header(
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

A `fastRhockey_data` tibble (47 columns; zero rows with the same columns
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
| home_club_id | integer | Home CLUB id (stable across seasons; joins webcaster team khl_id) |
| home_team_season_id | integer | Home team-season id (one per tournament; joins nothing in the webcaster API) |
| away_club_id | integer | Visitor CLUB id (stable across seasons; joins webcaster team khl_id) |
| away_team_season_id | integer | Visitor team-season id (one per tournament; joins nothing in the webcaster API) |
| arena_id | integer | khl.ru arena id |
| home_score | integer | Home goals (final, shootout winner counted) |
| away_score | integer | Visitor goals (final, shootout winner counted) |
| round | integer | Playoff round number |
| roundname | character | Round label in Russian ('Nedelya 29' = Week 29) |
| roundname_en | character | Round label in English |
| date | character | Game date YYYY-MM-DD (Moscow time) |
| dayofweek | integer | Day of week, 1 = Sunday (6 = Friday 2026-03-20) |
| time | character | Start time hh:mm:ss (Moscow time); not a game clock |
| changes | integer | Count of changes to the game record (0 in the captured bodies; unverified) |
| number | integer | Game number within the tournament |
| home_name_l3 | character | Home club 3-letter code in Russian |
| home_name_en | character | Home club name in English |
| home_name_l3_en | character | Home club 3-letter code in English |
| home_city_en | character | Home club city in English |
| visitor_name_l3 | character | Visiting club 3-letter code in Russian |
| visitor_name_en | character | Visiting club name in English |
| visitor_name_l3_en | character | Visiting club 3-letter code in English |
| visitor_city_en | character | Visiting club city in English |
| arena | character | Arena name in Russian |
| arena_en | character | Arena name in English |
| arena_city | character | Arena city in Russian |
| arena_city_en | character | Arena city in English |
| msktime_sft | integer | Offset of local time from Moscow time, hours (unverified) |
| gmttime_sft | integer | Offset of Moscow time from GMT, hours (unverified) |
| temp | character | Undocumented provider field `temp`; carried as sent. |
| score | character | Final score 'home:visitor' |
| ots | character | How the game ended: empty = regulation, OT = overtime, SO = shootout |
| attendance | integer | Attendance (numeric string) |
| approved | integer | 1 = result confirmed by the league |
| format_date_day | character | Game date YYYY-MM-DD (Moscow time), as the site formats it |
| time_format | character | Start time hh:mm (Moscow time) |
| full_date | integer | Start time, unix seconds (UTC) |
| date_type | character | Front-end date bucket of the game relative to today ('last' = already played; other values unverified) |
| type | character | Game state/type code (sogame = finished in a shootout) |
| winner | character | Winning side: A = home, B = visitor |
| visible_date | character | Short display date in Russian ('20 mar' = 20 March) |
| visible_date_full | character | Display date in Russian without the year ('20 marta') |
| visible_date_full2 | character | Display date in Russian with the year ('20 marta 2026') |
| visible_date_day | character | Abbreviated weekday in Russian ('pt' = Friday) |
| win | integer | True when this club won |

## Details

Endpoint: `POST https://www.khl.ru/rest/game/header/`.

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
  try(khl_game_header(game_id = 898236, season = 2026))
#> ── KHL Game Header from khl.ru ─────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:54:38 UTC
#> # A tibble: 1 × 47
#>   game_id tournament_id home_club_id home_team_season_id away_club_id
#>     <int>         <int>        <int>               <int>        <int>
#> 1  898236          1369            7               14934           71
#> # ℹ 42 more variables: away_team_season_id <int>, arena_id <int>,
#> #   home_score <int>, away_score <int>, round <int>, roundname <chr>,
#> #   roundname_en <chr>, date <chr>, dayofweek <int>, time <chr>, changes <int>,
#> #   number <int>, home_name_l3 <chr>, home_name_en <chr>,
#> #   home_name_l3_en <chr>, home_city_en <chr>, visitor_name_l3 <chr>,
#> #   visitor_name_en <chr>, visitor_name_l3_en <chr>, visitor_city_en <chr>,
#> #   arena <chr>, arena_en <chr>, arena_city <chr>, arena_city_en <chr>, …
# }
```
