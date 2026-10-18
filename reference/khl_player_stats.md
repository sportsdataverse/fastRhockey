# **KHL Player Stats**

League player stats of a tournament, one page (one row per player);
goalies come as one list of every page, sorted by games played.

## Usage

``` r
khl_player_stats(
  season = NULL,
  stage = "regular",
  position = NULL,
  club = NULL,
  page = 1,
  headers = NULL,
  tournament_id = NULL,
  per_page = 20,
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

- position:

  Position group: goalies, defenses, forwards, skaters or all. NULL =
  the site's default. One of `goalies`, `defenses`, `forwards`,
  `skaters`, `all`.

- club:

  Club slug (khl_stat_filters, filter clubs_full) or all. NULL = every
  club.

- page:

  1-based page. Loop until a page comes back shorter than per_page.
  Ignored for `position = "goalies"` (every page is fetched).

- headers:

  Named list of extra request headers, merged over the defaults
  (`X-Requested-With`, `Origin`, `Referer`). The cookie jar and `sessid`
  are kept in-process, so nothing needs reusing across calls.

- tournament_id:

  khl.ru tournament id (tnId), sent as is instead of season / stage
  (1369 = 2025-26 regular season, 1370 its playoff; the schedule's
  tournament_id column). The All-Star games have their own.

- per_page:

  Rows per page: 12, 20, 24 or 36. Keep 20: the site computes page
  counts and ranks for 20.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw khl.ru envelope as a list (`status`,
  `data`, `errors`; betting fields removed).

## Value

A `fastRhockey_data` tibble (45 columns; zero rows with the same columns
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
| rank | integer | 1-based position in its table / list |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| player_name | character | Player name as the site prints it (Russian on www.khl.ru) |
| image | character | Player photo URL on khl.ru (team-season specific) |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| gp | integer | Games played by the player |
| g | integer | Goals scored by the player |
| a | integer | Assists credited to the player |
| pts | integer | Points (goals plus assists) |
| pm | integer | Plus-minus rating of the player |
| plus | integer | Plus (on ice for a goal for) |
| minus | integer | Minus (on ice for a goal against) |
| pim | integer | Penalty minutes |
| eqg | integer | Even-strength goals |
| ppg | integer | Power-play goals |
| shg | integer | Short-handed goals |
| otg | integer | Overtime goals scored by the player |
| gwg | integer | Game-winning goals |
| sds | integer | Shootout-deciding goals |
| sog | integer | Shots on goal by the player |
| sog_pct | numeric | Shooting percentage |
| sog_avg | numeric | Shots on goal per game |
| fo | integer | Faceoffs taken by the player |
| fow | integer | Faceoffs won by the player |
| fo_pct | numeric | Faceoff win percentage |
| toi_avg | character | Average time on ice per game (mm:ss) |
| sft_avg | numeric | Average shifts per game |
| tie_avg | character | Average even-strength time on ice per game |
| sfte_avg | numeric | Average even-strength shifts per game |
| tipp_avg | character | Average power-play time on ice per game |
| sftpp_avg | numeric | Average power-play shifts per game |
| tish_avg | character | Average short-handed time on ice per game |
| sftsh_avg | numeric | Average short-handed shifts per game |
| hits | integer | Hits (body checks) |
| bls | integer | Shots blocked by the player |
| w | integer | Wins (goalies only) |
| l | integer | Losses (goalies only) |
| sop | integer | Games that went to a shootout (goalies only) |
| ga | integer | Goals against (goalies only) |
| sv | integer | Saves, shots on goal stopped (goalies only) |
| sv_pct | numeric | Save percentage on a 0-100 scale, e.g. 92.2 (goalies only) |
| gaa | numeric | Goals-against average, 60 min x goals against / time on ice (goalies only) |
| so | integer | Shutouts, games without a goal against (goalies only) |
| toi | character | Total time on the ice, mm:ss, e.g. "3521:21" (goalies only; NA when none) |

## Details

Endpoint: `POST https://www.khl.ru/rest/stat/players/`.

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

**Goalies.** `position = "goalies"` fetches every page (the page count
is the first page's `nav.max`) and returns them as one list sorted by
games played, then wins (both descending), then name and player link:
khl.ru orders goalies by goals-against average ascending and ignores
sort fields, so its page 1 is the goalies without a game. `page` is
ignored. A partial list (a missing page, a row count that does not fit
the page count, a player on two pages, more than 30 pages) raises
`fastRhockey_asset_fetch`. Skaters are one page, as asked.

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
  try(khl_player_stats(position = "forwards", season = 2026))
#> ── KHL Player Stats from khl.ru ────────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:16 UTC
#> # A tibble: 20 × 45
#>    group    rank player_id player_name image    jn    gp     g     a   pts    pm
#>    <chr>   <int>     <int> <chr>       <chr> <int> <int> <int> <int> <int> <int>
#>  1 forwar…     1     43033 Энэс Сэм    //im…     7    67    32    57    89    25
#>  2 forwar…     2     18770 Окулов Кон… //im…    17    68    24    50    74    29
#>  3 forwar…     3     45401 Лимож Алекс //im…    12    68    24    49    73    31
#>  4 forwar…     4     19014 Ткачёв Вла… //im…    19    66    13    58    71    19
#>  5 forwar…     5     34820 Пинчук Вит… //im…    81    65    31    35    66    21
#>  6 forwar…     6     45426 Потуральск… //im…    22    67    27    39    66    29
#>  7 forwar…     7     43403 Ливо Джошуа //im…    90    62    26    39    65    15
#>  8 forwar…     8     39443 Канцеров Р… //im…     8    63    36    28    64    22
#>  9 forwar…     9     45421 Спронг Дан… //im…    11    52    30    34    64    15
#> 10 forwar…    10     16874 Григоренко… //im…    94    62    24    35    59     4
#> 11 forwar…    11     17695 Семёнов Ки… //im…    94    68    18    38    56    22
#> 12 forwar…    12     16006 Гусев Ники… //im…    97    66    17    38    55     7
#> 13 forwar…    13     23540 Уил Джордан //im…    91    67    14    41    55     3
#> 14 forwar…    14     40272 Виноградов… //im…    92    67    23    31    54     6
#> 15 forwar…    15     40292 Тодд Нэйтан //im…    29    61    22    31    53     1
#> 16 forwar…    16       496 Радулов Ал… //im…    47    64    21    31    52    31
#> 17 forwar…    17     17210 Барабанов … //im…    17    64    21    30    51    12
#> 18 forwar…    18     21246 Хайруллин … //im…    61    67    19    30    49     9
#> 19 forwar…    19     30629 Абросимов … //im…    17    62    24    24    48    15
#> 20 forwar…    20     20700 Белозёров … //im…    71    62    27    21    48     4
#> # ℹ 34 more variables: plus <int>, minus <int>, pim <int>, eqg <int>,
#> #   ppg <int>, shg <int>, otg <int>, gwg <int>, sds <int>, sog <int>,
#> #   sog_pct <dbl>, sog_avg <dbl>, fo <int>, fow <int>, fo_pct <dbl>,
#> #   toi_avg <chr>, sft_avg <dbl>, tie_avg <chr>, sfte_avg <dbl>,
#> #   tipp_avg <chr>, sftpp_avg <dbl>, tish_avg <chr>, sftsh_avg <dbl>,
#> #   hits <int>, bls <int>, w <int>, l <int>, sop <int>, ga <int>, sv <int>,
#> #   sv_pct <dbl>, gaa <dbl>, so <int>, toi <chr>
# }
```
