# **KHL Player Profile**

Player profile: bio, contract, club (one row).

## Usage

``` r
khl_player_profile(player_id, headers = NULL, return_parsed = TRUE)
```

## Arguments

- player_id:

  Required. khl.ru player id (the box score / roster player_id).

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

## Value

A `fastRhockey_data` tibble (31 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| firstname | character | First name in Russian |
| lastname | character | Last name in Russian |
| photo | character | Player photo URL on khl.ru (protocol-relative) |
| position | character | Position code (Russian letter on www.khl.ru) |
| position_code | character | Position key in English (goaltender, defender, forward) |
| biography | character | Biography text in Russian (HTML) |
| is_hero | logical | Site 'hero' flag (unverified meaning) |
| is_rip | logical | True for a deceased player |
| links_stats | character | Relative khl.ru page path |
| pagelink | character | Relative khl.ru page path |
| summary_stats_gp | integer | Career KHL games played (profile summary) |
| summary_stats_a | integer | Career KHL assists (profile summary) |
| summary_stats_pm | integer | Career KHL plus-minus (profile summary) |
| summary_stats_g | integer | Career KHL goals (profile summary) |
| summary_stats_pts | integer | Career KHL points (profile summary) |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| club_name | character | Name in Russian (Cyrillic); the \*\_en / \*en sibling holds the English transliteration |
| club_logo | character | Current club logo URL on khl.ru (protocol-relative) |
| club_code | character | Club slug (/clubs//) |
| height | character | Height with its Russian unit ('183 sm' = 183 cm) |
| weight | character | Weight with its Russian unit ('85 kg' = 85 kg) |
| birthdate | character | Birth date (Russian display format) |
| age | character | Age with its Russian unit ('27 let' = 27 years) |
| contract_name | character | Contract label in Russian |
| contract_date | character | Contract end date |
| country_name | character | Name in Russian (Cyrillic); the \*\_en / \*en sibling holds the English transliteration |
| country_flag | character | Flag image path of the player's country (citizenship) |
| sport_country_name | character | Name in Russian (Cyrillic); the \*\_en / \*en sibling holds the English transliteration |
| sport_country_flag | character | Flag image path of the country the player represents in sport |
| stick | character | Shooting hand: l = left, r = right |

## Details

Endpoint: `POST https://www.khl.ru/rest/players/profile/`.

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
  try(khl_player_profile(player_id = 41928))
#> ── KHL Player Profile from khl.ru ──────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:15 UTC
#> # A tibble: 1 × 31
#>   player_id firstname lastname photo    position position_code biography is_hero
#>       <int> <chr>     <chr>    <chr>    <chr>    <chr>         <chr>     <lgl>  
#> 1     41928 Джозеф    Кин      //img.k… защитник defender      ""        FALSE  
#> # ℹ 23 more variables: is_rip <lgl>, links_stats <chr>, pagelink <chr>,
#> #   summary_stats_gp <int>, summary_stats_a <int>, summary_stats_pm <int>,
#> #   summary_stats_g <int>, summary_stats_pts <int>, jn <int>, club_name <chr>,
#> #   club_logo <chr>, club_code <chr>, height <chr>, weight <chr>,
#> #   birthdate <chr>, age <chr>, contract_name <chr>, contract_date <chr>,
#> #   country_name <chr>, country_flag <chr>, sport_country_name <chr>,
#> #   sport_country_flag <chr>, stick <chr>
# }
```
