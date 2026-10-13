# **KHL Playoff Bracket**

Playoff bracket: one row per team per series.

## Usage

``` r
khl_playoff_bracket(
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
  Required unless tournament_id is given.

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

A `fastRhockey_data` tibble (9 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| round | integer | Playoff round number |
| wins_to_advance | integer | Wins needed to win a series of the round |
| pair | integer | Series number within the round |
| conference | character | Conference (west / east) |
| seed | integer | Conference seed |
| club_id | integer | khl.ru CLUB id (stable across seasons; joins webcaster team khl_id) |
| name | character | Name in Russian (Cyrillic); the \*\_en / \*en sibling holds the English transliteration |
| series_wins | integer | Wins in the series so far |
| game_ids | character | khl.ru game ids of the series, comma-joined |

## Details

Endpoint: `POST https://www.khl.ru/rest/standings/playoff/`.

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
  try(khl_playoff_bracket(season = 2026))
#> ── KHL Playoff Bracket from khl.ru ─────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-13 01:54:53 UTC
#> # A tibble: 30 × 9
#>    round wins_to_advance  pair conference  seed club_id name         series_wins
#>    <int>           <int> <int> <chr>      <int>   <int> <chr>              <int>
#>  1     1               4     1 west           1       1 Локомотив              4
#>  2     1               4     1 west           8       7 Спартак                1
#>  3     1               4     2 east           4     190 Автомобилист           2
#>  4     1               4     2 east           5      38 Салават Юла…           4
#>  5     1               4     3 east           2      34 Авангард               4
#>  6     1               4     3 east           7      71 Нефтехимик             1
#>  7     1               4     4 west           4       2 ЦСКА                   4
#>  8     1               4     4 west           5      24 СКА                    1
#>  9     1               4     5 east           1      37 Металлург Мг           4
#> 10     1               4     5 east           8      29 Сибирь                 1
#> # ℹ 20 more rows
#> # ℹ 1 more variable: game_ids <chr>
# }
```
