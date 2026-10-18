# **KHL Standings**

Regular-season standings: one row per team per table (league,
conferences, divisions).

## Usage

``` r
khl_standings(
  season = NULL,
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

A `fastRhockey_data` tibble (17 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| table | character | Standings table: league, a conference (west / east) or a division key |
| rank | integer | 1-based position in its table / list |
| club_id | integer | khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
| name | character | Name in Russian (Cyrillic); the \*\_en / \*en sibling holds the English transliteration |
| gp | integer | Games played (gp = w + otw + sow + sol + otl + l) |
| w | integer | Regulation wins only: overtime (otw) and shootout (sow) wins are separate columns |
| otw | integer | Overtime wins (not counted in w) |
| sow | integer | Shootout wins (not counted in w) |
| sol | integer | Shootout losses |
| otl | integer | Overtime losses |
| otl_en | integer | Overtime losses with the net empty |
| l | integer | Regulation losses only: overtime (otl) and shootout (sol) losses are separate columns |
| pts | integer | Points: 2 per win of any kind, 1 per overtime or shootout loss (3-2-1 in early seasons such as 2013-14) |
| goals_for | integer | Goals scored by the team |
| goals_against | integer | Goals allowed by the team |
| is_out_playoff | logical | Eliminated from playoff contention |
| link | character | Relative khl.ru page path |

## Details

Endpoint: `POST https://www.khl.ru/rest/standings/regular/`.

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
[`khl_shots()`](https://fastRhockey.sportsdataverse.org/reference/khl_shots.md),
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
  try(khl_standings(season = 2026))
#> ── KHL Standings from khl.ru ───────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:47:04 UTC
#> # A tibble: 69 × 17
#>    table   rank club_id name       gp     w   otw   sow   sol   otl otl_en     l
#>    <chr>  <int>   <int> <chr>   <int> <int> <int> <int> <int> <int>  <int> <int>
#>  1 league     1      37 Металл…    68    37     8     4     3     4      0    12
#>  2 league     2      34 Аванга…    68    36     6     5     1     4      0    16
#>  3 league     3       1 Локомо…    68    34     6     6     2     4      0    16
#>  4 league     4      53 Ак Барс    68    38     2     3     6     2      0    17
#>  5 league     5     207 Динамо…    68    33     2     4     5     5      0    19
#>  6 league     6      56 Северс…    68    29     9     2     4     2      0    22
#>  7 league     7       2 ЦСКА       68    33     3     2     7     1      0    22
#>  8 league     8     190 Автомо…    68    32     4     4     0     4      0    24
#>  9 league    NA      NA NA         NA    NA    NA    NA    NA    NA     NA    NA
#> 10 league     9      24 СКА        68    28     7     2     4     3      0    24
#> # ℹ 59 more rows
#> # ℹ 5 more variables: pts <int>, goals_for <int>, goals_against <int>,
#> #   is_out_playoff <lgl>, link <chr>
# }
```
