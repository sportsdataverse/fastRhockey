# **KHL Webcaster Events**

Games of a stage, 16 per page (one row per game; khl_id is the khl.ru
game id).

## Usage

``` r
khl_webcaster_events(
  season = NULL,
  stage = "regular",
  page = 1,
  stage_id = NULL,
  team_ids = NULL,
  start_after = NULL,
  start_before = NULL,
  locale = "en",
  return_parsed = TRUE
)
```

## Arguments

- season:

  Season by its END year (2026 = 2025-26), mapped to the KHL webcaster
  stage id through the webcaster season table (khl_webcaster_stages).
  NULL = the current stage.

- stage:

  Which stage of the season: regular or playoff. One of `regular`,
  `playoff`.

- page:

  1-based page of 16 games; an empty page ends the stage.

- stage_id:

  Webcaster stage id, sent as is instead of season / stage (370 =
  2025-26 regular season; khl_webcaster_stages lists them). Not a khl.ru
  tournament id.

- team_ids:

  Webcaster team id(s) (khl_webcaster_teams id, not a khl.ru club id):
  games of these teams.

- start_after:

  Games starting after this unix time (seconds). Needs a stage.

- start_before:

  Games starting before this unix time (seconds). Needs a stage.

- locale:

  Response language: en or ru. One of `en`, `ru`.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw JSON body as a list (betting fields
  and the caller's IP removed).

## Value

A `fastRhockey_data` tibble (54 columns; zero rows with the same columns
when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_state_key | character | Game state key (only finished observed) |
| period | integer | Period number (box score: 0 = the whole game; 4+ = overtime) |
| hd | logical | HD video available |
| id | integer | Webcaster id of the `event` object (webcaster key space); join to khl.ru through its khl_id |
| stage_id | integer | Webcaster stage id (its own id space; not a khl.ru tournament id) |
| type_id | integer | Webcaster event type code |
| commentator | logical | Commentary track available |
| likes_enabled | logical | True when likes are enabled on the broadcast page |
| is_vertical_video | logical | True when the broadcast video is vertical (portrait) |
| khl_id | integer | khl.ru game id of the `event` object (site key space): the join key to khl-rest |
| team_a_id | integer | Webcaster id of the `team_a` object (webcaster key space); join to khl.ru through its khl_id |
| team_a_khl_id | integer | khl.ru club id of the `team_a` object (site key space): the join key to khl-rest |
| team_a_image | character | Home team logo URL (webcaster thumbnail) |
| team_a_name | character | Home team name (English with locale=en) |
| team_a_location | character | Home team city / region (English with locale=en) |
| team_b_id | integer | Webcaster id of the `team_b` object (webcaster key space); join to khl.ru through its khl_id |
| team_b_khl_id | integer | khl.ru club id of the `team_b` object (site key space): the join key to khl-rest |
| team_b_image | character | Visitor team logo URL (webcaster thumbnail) |
| team_b_name | character | Visitor team name (English with locale=en) |
| team_b_location | character | Visitor team city / region (English with locale=en) |
| name | character | Matchup label in English |
| tickets | character | Undocumented provider field `tickets`; carried as sent. |
| location | character | Home team city / region (English with locale=en) |
| start_at_day | integer | Game day start (midnight Moscow time), unix seconds |
| start_at | integer | Start time, unix milliseconds |
| event_start_at | integer | Puck-drop time, unix milliseconds |
| end_at | integer | End time, unix milliseconds |
| not_regular | logical | True for a game outside the regular schedule (False in the captured bodies; unverified) |
| score | character | Final score 'team_a:team_b' |
| scores_first_period | character | 1st-period score 'a:b' |
| scores_second_period | character | 2nd-period score 'a:b' |
| scores_third_period | character | 3rd-period score 'a:b' |
| scores_overtime | character | Overtime score 'a:b' (null if none) |
| scores_bullitt | character | Shootout score 'a:b' (null if none); 'bullitt' is Russian for shootout |
| sscore | character | Undocumented provider field `sscore`; carried as sent. |
| image | character | Broadcast thumbnail image URL |
| condensed_game_id | integer | Webcaster event id of the condensed-game video |
| highlight_id | integer | Webcaster event id of the highlights video |
| infographics | character | Scoreboard overlay label of the broadcast (With scoreboard) |
| infographics_enabled | logical | True when the broadcast carries the scoreboard overlay |
| has_video | logical | Video available |
| m3u8_url | character | Broadcast HLS (m3u8) stream URL; empty without rights |
| feed_url | character | Broadcast webcaster player feed URL; empty without rights |
| iframe_url | character | Broadcast embeddable player (iframe) URL |
| iframe_code | character | Broadcast embed HTML (an iframe tag) |
| free | logical | Video free to watch |
| yandex_rights | logical | Yandex broadcast rights flag |
| yandex_id | character | Yandex video id of the broadcast (m-prefixed) |
| match_id | integer | khl.ru game id as a string (= khl_id) |
| balancer_type | character | Video delivery type (record = archived broadcast) |
| event_or_quote_type_name | character | Video category label in Russian (Arkhiv = archive) |
| outer_stage_id | integer | khl.ru tournament id of the event's stage (= stage.khl_id) |
| stage_name | character | Stage label (Regular 2025/2026) |
| previews | character | Undocumented provider field `previews`; carried as sent. |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/events_v2.json`.

The webcaster API is keyless JSON. Neither backend takes a year:
`season` (the END year, 2026 = 2025/26) and `stage` are mapped to the
webcaster stage id through the season table of
[`khl_webcaster_stages()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_stages.md)
(cached for the R session), or pass `stage_id`. Honours
`getOption("fastRhockey.proxy")`; betting fields and the caller's IP are
removed from every body.

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
[`khl_standings()`](https://fastRhockey.sportsdataverse.org/reference/khl_standings.md),
[`khl_stat_filters()`](https://fastRhockey.sportsdataverse.org/reference/khl_stat_filters.md),
[`khl_webcaster_event()`](https://fastRhockey.sportsdataverse.org/reference/khl_webcaster_event.md),
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
  try(khl_webcaster_events(season = 2026))
#> ── KHL Webcaster Events from khl.api.webcaster.pro ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:13:30 UTC
#> # A tibble: 16 × 54
#>    game_state_key period hd        id stage_id type_id commentator likes_enabled
#>    <chr>           <int> <lgl>  <int>    <int>   <int> <lgl>       <lgl>        
#>  1 finished           -1 TRUE  2.79e6      370      18 TRUE        FALSE        
#>  2 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#>  3 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#>  4 finished           -1 TRUE  2.78e6      370      18 TRUE        NA           
#>  5 finished           -1 TRUE  2.79e6      370      18 TRUE        FALSE        
#>  6 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#>  7 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#>  8 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#>  9 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 10 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 11 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 12 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 13 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 14 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 15 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> 16 finished           -1 TRUE  2.79e6      370      18 TRUE        NA           
#> # ℹ 46 more variables: is_vertical_video <lgl>, khl_id <int>, team_a_id <int>,
#> #   team_a_khl_id <int>, team_a_image <chr>, team_a_name <chr>,
#> #   team_a_location <chr>, team_b_id <int>, team_b_khl_id <int>,
#> #   team_b_image <chr>, team_b_name <chr>, team_b_location <chr>, name <chr>,
#> #   tickets <chr>, location <chr>, start_at_day <int>, start_at <dbl>,
#> #   event_start_at <dbl>, end_at <dbl>, not_regular <lgl>, score <chr>,
#> #   scores_first_period <chr>, scores_second_period <chr>, …
# }
```
