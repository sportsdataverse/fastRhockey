# **KHL Roster**

Club roster (current season only): one row per player.

## Usage

``` r
khl_roster(club_id, headers = NULL, return_parsed = TRUE)
```

## Arguments

- club_id:

  Required. khl.ru CLUB id (7 = Spartak; schedule home_club_id), not a
  team-season id.

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

## Value

A `fastRhockey_data` tibble (10 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| group | character | List the row comes from (position group / roster section) |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| photo | character | Player photo URL on khl.ru (protocol-relative) |
| country | character | Citizenship (Russian) |
| lastname | character | Last name in Russian |
| firstname | character | First name in Russian |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| age | integer | Player age in years |
| favorite | logical | Front-end favourite flag of the signed-in user (always False here) |
| link | character | Relative khl.ru page path |

## Details

Endpoint: `POST https://www.khl.ru/rest/clubs/team/`.

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
  try(khl_roster(club_id = 7))
#> ── KHL Roster from khl.ru ──────────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:20 UTC
#> # A tibble: 31 × 10
#>    group   player_id photo country lastname firstname    jn   age favorite link 
#>    <chr>       <int> <chr> <chr>   <chr>    <chr>     <int> <int> <lgl>    <chr>
#>  1 goalke…     19184 //im… Россия  Загидул… Артём        50    31 FALSE    /pla…
#>  2 goalke…     45157 //im… Россия  Кайрис   Антон        23    19 FALSE    /pla…
#>  3 goalke…     20870 //im… Россия  Федотов  Иван         28    29 FALSE    /pla…
#>  4 defend…     15299 //im… Россия  Вишневс… Дмитрий      55    36 FALSE    /pla…
#>  5 defend…     43948 //im… Россия  Дронов   Николай      49    18 FALSE    /pla…
#>  6 defend…     41928 //im… США     Кин      Джозеф        4    27 FALSE    /pla…
#>  7 defend…     29100 //im… Россия  Князев   Артемий       8    25 FALSE    /pla…
#>  8 defend…     38354 //im… Россия  Королёв  Вениамин     89    23 FALSE    /pla…
#>  9 defend…     17624 //im… Россия  Миронов  Андрей А.    94    32 FALSE    /pla…
#> 10 defend…     39327 //im… Россия  Орлов    Даниил Р.    62    22 FALSE    /pla…
#> # ℹ 21 more rows
# }
```
