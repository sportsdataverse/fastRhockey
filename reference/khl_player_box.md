# **KHL Player Box Score**

Skater box score by period: one row per period, side and jersey, with
the player id.

## Usage

``` r
khl_player_box(
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

A `fastRhockey_data` tibble (30 columns; zero rows with the same columns
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
| team_side | character | A = home, B = visitor |
| jn | integer | Jersey number (not an id; 0 on a team event) |
| player_id | integer | khl.ru player id; equals webcaster player khl_id |
| player_name | character | Player name as the site prints it (Russian on www.khl.ru) |
| position | character | Position code (Russian letter on www.khl.ru) |
| g | integer | Goals scored in the period block |
| a | integer | Home roster keyed by jersey number |
| p | character | Points (empty string when none) |
| shots | integer | Shots on goal in the period block |
| pim | integer | Penalty minutes |
| fo | integer | Faceoffs taken in the period block |
| fow | integer | Faceoffs won in the period block |
| fow_pct | numeric | Stat code the bar is computed from |
| bls | integer | Shots blocked in the period block |
| hits | integer | Hits (body checks) |
| fa | integer | Takeaways (the site labels this column 'Otbory', takeaways) |
| p_intc | integer | Pass interceptions |
| sft | character | Time on ice 'mm:ss(shifts)' |
| sfte | character | Even-strength time on ice 'mm:ss(shifts)' |
| sftpp | character | Power-play time on ice 'mm:ss(shifts)' |
| sftsh | character | Short-handed time on ice 'mm:ss(shifts)' |
| sften | character | Empty-net time on ice 'mm:ss(shifts)' |
| tiams | character | Time in the attacking zone (mm:ss) |
| ts | numeric | Top speed, km/h (tracking; present by 2019/20) |
| dt | numeric | Distance skated, km (tracking) |
| as | numeric | Average speed, km/h (tracking) |
| pcd | numeric | Puck-control duration (tracking; unit unverified) |

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
  try(khl_player_box(game_id = 898236, season = 2026))
#> ── KHL Player Box Score from khl.ru ────────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 05:46:48 UTC
#> # A tibble: 190 × 30
#>    game_id tournament_id period team_side    jn player_id player_name   position
#>      <int>         <int>  <int> <chr>     <int>     <int> <chr>         <chr>   
#>  1  898236          1369      0 A             4     41928 Кин Джозеф    з       
#>  2  898236          1369      0 A             8     29100 Князев Артем… з       
#>  3  898236          1369      0 A            33     35680 Соболев Дани… з       
#>  4  898236          1369      0 A            55     15299 Вишневский Д… з       
#>  5  898236          1369      0 A            78     30201 Бычков Роман  з       
#>  6  898236          1369      0 A            94     17624 Миронов Андр… з       
#>  7  898236          1369      0 A            13     23461 Мальцев Миха… н       
#>  8  898236          1369      0 A            16     40673 Пивчулин Дан… н       
#>  9  898236          1369      0 A            17     26742 Морозов Иван… н       
#> 10  898236          1369      0 A            20     27992 Локхарт Лукас н       
#> # ℹ 180 more rows
#> # ℹ 22 more variables: g <int>, a <int>, p <chr>, shots <int>, pim <int>,
#> #   fo <int>, fow <int>, fow_pct <dbl>, bls <int>, hits <int>, fa <int>,
#> #   p_intc <int>, sft <chr>, sfte <chr>, sftpp <chr>, sftsh <chr>, sften <chr>,
#> #   tiams <chr>, ts <dbl>, dt <dbl>, as <dbl>, pcd <dbl>
# }
```
