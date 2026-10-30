# **KHL Webcaster Event**

One game: officials, team totals, per-player match stats, goals,
penalties (one row; lists as JSON text).

## Usage

``` r
khl_webcaster_event(event_id, locale = "en", return_parsed = TRUE)
```

## Arguments

- event_id:

  Required. Webcaster event id (khl_webcaster_events id), not the khl.ru
  game id.

- locale:

  Response language: en or ru. One of `en`, `ru`.

- return_parsed:

  Logical. `TRUE` (default) returns the parsed `fastRhockey_data`
  tibble; `FALSE` returns the raw JSON body as a list (betting fields
  and the caller's IP removed).

## Value

A `fastRhockey_data` tibble (159 columns; zero rows with the same
columns when the payload is empty), or the raw body as a list when
`return_parsed = FALSE`. Raises `fastRhockey_no_data` (nothing there, or
an unknown season / stage), `fastRhockey_asset_fetch` (an error
envelope, an answer for another id than the one asked, or a failed
fetch) and `fastRhockey_bad_request` (an invalid argument or an HTTP 400
/ 422).

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_state_key | character | Game state key (finished, ...) |
| period | integer | Period number (box score: 0 = the whole game; 4+ = overtime) |
| hd | logical | True when the broadcast is in HD |
| id | integer | Webcaster id of the `arena` object (webcaster key space); no khl.ru counterpart in the body |
| season | character | Season END year (2026 = 2025-26) |
| stage_id | integer | Webcaster stage id (its own id space; not a khl.ru tournament id) |
| type_id | integer | Webcaster event type code |
| commentator | logical | True when the broadcast has a commentator |
| likes_enabled | logical | True when likes are enabled on the broadcast page |
| is_vertical_video | logical | True when the broadcast video is vertical (portrait) |
| khl_id | integer | khl.ru game id of the `event` object (site key space): the join key to khl-rest |
| arena_id | integer | khl.ru arena id |
| arena_capacity | integer | Arena seating capacity |
| arena_phone | character | Arena phone number as published |
| arena_website | character | Arena website URL |
| arena_name | character | Arena (city) name, from data.ARENAS |
| arena_address | character | Arena street address (English with locale=en) |
| arena_city | character | Arena city (English with locale=en) |
| arena_image | character | Arena photo URL on khl.ru |
| arena_geo_lat | numeric | Arena latitude, decimal degrees |
| arena_geo_long | numeric | Arena longitude, decimal degrees |
| team_a_id | integer | Webcaster id of the `team_a` object (webcaster key space); join to khl.ru through its khl_id |
| team_a_khl_id | integer | khl.ru club id of the `team_a` object (site key space): the join key to khl-rest |
| team_a_image | character | Home team logo URL (webcaster thumbnail) |
| team_a_name | character | Home team name (English with locale=en) |
| team_a_location | character | Home team city / region (English with locale=en) |
| team_a_shots | integer | Home shots on goal |
| team_a_gf | integer | Home team goals (final) |
| team_a_ppg | integer | Power-play goals |
| team_a_shg | integer | Short-handed goals |
| team_a_ppc | integer | Count; equals the team's own penalties in the sample (5 for 10 PIM), so probably penalties taken. UNVERIFIED |
| team_a_vbr | integer | Faceoffs won ('vbr' from Russian vbrasyvaniya; = game/text fow) |
| team_a_pim | integer | Penalty minutes |
| team_a_total_puck_control_time | numeric | Team puck-control time, decimal MINUTES (21.81 = 21:48) |
| team_a_total_distance_travelled | numeric | Distance skated by the team, km (= game/text dtA) |
| team_a_offensive_blue_line_crossings_count | integer | Offensive blue-line crossings |
| team_a_top_players | character | Home game leaders (hardest shot, ...), JSON list with the webcaster and khl.ru player ids |
| team_a_start_fives | character | Home starting lineup, JSON list of players (webcaster id, khl_id, jersey, role) |
| team_a_players | character | Home game roster, JSON list of players (webcaster id, khl_id, jersey, role) |
| team_a_active | logical | True when the home team is active in the webcaster app |
| team_b_id | integer | Webcaster id of the `team_b` object (webcaster key space); join to khl.ru through its khl_id |
| team_b_khl_id | integer | khl.ru club id of the `team_b` object (site key space): the join key to khl-rest |
| team_b_image | character | Visitor team logo URL (webcaster thumbnail) |
| team_b_name | character | Visitor team name (English with locale=en) |
| team_b_location | character | Visitor team city / region (English with locale=en) |
| team_b_shots | integer | Visitor shots on goal |
| team_b_gf | integer | Visitor team goals (final) |
| team_b_ppg | integer | Power-play goals |
| team_b_shg | integer | Short-handed goals |
| team_b_ppc | integer | Count; equals the team's own penalties in the sample (3 for 6 PIM), so probably penalties taken. UNVERIFIED |
| team_b_vbr | integer | Faceoffs won ('vbr' from Russian vbrasyvaniya; = game/text fow) |
| team_b_pim | integer | Penalty minutes |
| team_b_total_puck_control_time | numeric | Team puck-control time, decimal MINUTES (21.81 = 21:48) |
| team_b_total_distance_travelled | numeric | Distance skated by the team, km (= game/text dtA) |
| team_b_offensive_blue_line_crossings_count | integer | Offensive blue-line crossings |
| team_b_top_players | character | Visitor game leaders (hardest shot, ...), JSON list with the webcaster and khl.ru player ids |
| team_b_start_fives | character | Visitor starting lineup, JSON list of players (webcaster id, khl_id, jersey, role) |
| team_b_players | character | Visitor game roster, JSON list of players (webcaster id, khl_id, jersey, role) |
| team_b_active | logical | True when the visitor team is active in the webcaster app |
| quotes | character | Video clips (webcaster quotes) |
| goals | character | Goals of the game, JSON list (time in game seconds, score, status, author, assistants) |
| violations | character | Penalties in order |
| other_events_with_both_teams | character | Other games between the two teams, JSON list of webcaster events |
| social_tags | character | Social-media hashtags of the game, JSON list |
| free | logical | True when the broadcast is free to watch (no subscription) |
| yandex_rights | logical | True when Yandex holds rights to the broadcast (unverified) |
| yandex_id | character | Yandex video id of the broadcast (m-prefixed) |
| match_id | integer | khl.ru game id as a string (= khl_id) |
| balancer_type | character | Video delivery type (record = archived broadcast) |
| event_or_quote_type_name | character | Video category label in Russian (Arkhiv = archive) |
| outer_stage_id | integer | khl.ru tournament id of the event's stage (= stage.khl_id) |
| stage_name | character | Stage label (Regular 2025/2026) |
| parent_id | integer | Undocumented provider field `parent_id`; carried as sent. |
| name | character | Name (English with locale=en) |
| mref1 | character | Referee 1, name in Russian |
| mref2 | character | Referee 2, name in Russian |
| lref1 | character | Linesman 1, name in Russian |
| lref2 | character | Linesman 2, name in Russian |
| announce | character | Undocumented provider field `announce`; carried as sent. |
| views | character | Broadcast view count as display text ('7,579') |
| stage_type | character | Stage type: regular or playoff |
| audio_tracks | character | Broadcast audio tracks, JSON list of id / title (main, intersound) |
| image_big | character | Large broadcast thumbnail image URL |
| location | character | Home team city / region (English with locale=en) |
| start_at_day | integer | Game day start (midnight Moscow time), unix seconds |
| start_at | integer | Start time, unix milliseconds |
| event_start_at | integer | Puck-drop time, unix milliseconds |
| end_at | integer | End time, unix milliseconds |
| tickets | character | Undocumented provider field `tickets`; carried as sent. |
| text_events | character | Summary text events only; no shot-level play-by-play |
| not_regular | logical | True for a game outside the regular schedule (False in the captured bodies; unverified) |
| infographics | character | Scoreboard overlay label of the broadcast (With scoreboard) |
| infographics_enabled | logical | True when the broadcast carries the scoreboard overlay |
| score | character | Score after the goal 'a:b' |
| condensed_game_id | integer | Webcaster id of the `condensed_game` object (webcaster key space); no khl.ru counterpart in the body |
| condensed_game_name | character | Condensed-game video title |
| condensed_game_start_ts | integer | Condensed-game video start time, unix milliseconds |
| condensed_game_m3u8_url | character | Condensed-game video HLS (m3u8) stream URL; signed and expiring |
| condensed_game_feed_url | character | Condensed-game video webcaster player feed URL; signed and expiring |
| condensed_game_iframe_url | character | Condensed-game video embeddable player (iframe) URL |
| condensed_game_iframe_code | character | Condensed-game video embed HTML (an iframe tag) |
| condensed_game_finish_ts | integer | Condensed-game video end time, unix milliseconds |
| condensed_game_image_url | character | Condensed-game video thumbnail image URL |
| condensed_game_description | character | Condensed-game video description text (HTML-escaped; Russian goal list on highlights) |
| condensed_game_audio_tracks | character | Condensed-game video audio tracks, JSON list of id / title (main, intersound) |
| condensed_game_is_vertical_video | logical | Condensed-game video: True when the video is vertical (portrait) |
| condensed_game_stage_id | integer | Condensed-game video: webcaster stage id (its own id space; not a khl.ru tournament id) |
| condensed_game_free | logical | Condensed-game video: True when free to watch (no subscription) |
| condensed_game_yandex_rights | logical | Condensed-game video: True when Yandex holds rights to the video (unverified) |
| condensed_game_yandex_id | character | Condensed-game video: Yandex video id of the broadcast (m-prefixed) |
| condensed_game_match_id | integer | khl.ru game id as a string (= khl_id) |
| condensed_game_balancer_type | character | Condensed-game video: video delivery type (record = archived broadcast, file = clip) |
| condensed_game_event_or_quote_type_name | character | Condensed-game video: video category label (Arkhiv = archive, Khaylayty = highlights) |
| condensed_game_outer_stage_id | integer | khl.ru tournament id of the event's stage (= stage.khl_id) |
| condensed_game_stage_name | character | Condensed-game video: stage label (Regular 2025/2026) |
| highlight_id | integer | Webcaster id of the `highlight` object (webcaster key space); no khl.ru counterpart in the body |
| highlight_name | character | Highlights video title |
| highlight_start_ts | integer | Highlights video start time, unix milliseconds |
| highlight_m3u8_url | character | Highlights video HLS (m3u8) stream URL; signed and expiring |
| highlight_feed_url | character | Highlights video webcaster player feed URL; signed and expiring |
| highlight_iframe_url | character | Highlights video embeddable player (iframe) URL |
| highlight_iframe_code | character | Highlights video embed HTML (an iframe tag) |
| highlight_finish_ts | integer | Highlights video end time, unix milliseconds |
| highlight_image_url | character | Highlights video thumbnail image URL |
| highlight_description | character | Highlights video description text (HTML-escaped; Russian goal list on highlights) |
| highlight_audio_tracks | character | Highlights video audio tracks, JSON list of id / title (main, intersound) |
| highlight_is_vertical_video | logical | Highlights video: True when the video is vertical (portrait) |
| highlight_stage_id | integer | Highlights video: webcaster stage id (its own id space; not a khl.ru tournament id) |
| highlight_free | logical | Highlights video: True when free to watch (no subscription) |
| highlight_yandex_rights | logical | Highlights video: True when Yandex holds rights to the video (unverified) |
| highlight_yandex_id | character | Highlights video: Yandex video id of the broadcast (m-prefixed) |
| highlight_match_id | integer | khl.ru game id as a string (= khl_id) |
| highlight_balancer_type | character | Highlights video: video delivery type (record = archived broadcast, file = clip) |
| highlight_event_or_quote_type_name | character | Highlights video: video category label (Arkhiv = archive, Khaylayty = highlights) |
| highlight_outer_stage_id | integer | khl.ru tournament id of the event's stage (= stage.khl_id) |
| highlight_stage_name | character | Highlights video: stage label (Regular 2025/2026) |
| outer_url | character | khl.ru page of the game (the game_id and tournament_id are in its path) |
| m3u8_url | character | Broadcast HLS (m3u8) stream URL; empty without rights |
| feed_url | character | Broadcast webcaster player feed URL; empty without rights |
| iframe_url | character | Broadcast embeddable player (iframe) URL |
| iframe_code | character | Broadcast embed HTML (an iframe tag) |
| scores_first_period | character | 1st-period score 'a:b' |
| scores_second_period | character | 2nd-period score 'a:b' |
| scores_third_period | character | 3rd-period score 'a:b' |
| scores_overtime | character | Overtime score 'a:b' (null if none) |
| scores_bullitt | character | Shootout score 'a:b' (null if none); 'bullitt' is Russian for shootout |
| this_pair_stat_events_count | integer | Head-to-head: games between the two teams on record (scope unverified) |
| this_pair_stat_team_a_wins_count | integer | Head-to-head: home team wins in those games |
| this_pair_stat_team_a_goals_count | integer | Head-to-head: home team goals in those games |
| this_pair_stat_team_a_points_count | integer | Head-to-head: home team standings points in those games |
| this_pair_stat_team_b_wins_count | integer | Head-to-head: visitor team wins in those games |
| this_pair_stat_team_b_goals_count | integer | Head-to-head: visitor team goals in those games |
| this_pair_stat_team_b_points_count | integer | Head-to-head: visitor team standings points in those games |
| transaction_types | character | Paid video products (app store) |
| sscore | character | Undocumented provider field `sscore`; carried as sent. |
| commentators_names | character | Broadcast commentators, comma-separated |
| transactions | character | Undocumented provider field `transactions`; carried as sent. |
| has_video | logical | True when the broadcast has a video |
| geo_error | character | Undocumented provider field `geo_error`; carried as sent. |

## Details

Endpoint:
`GET https://khl.api.webcaster.pro/api/khl_mobile/event_v2.json`.

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
  try(khl_webcaster_event(event_id = 2786714))
#> ── KHL Webcaster Event from khl.api.webcaster.pro ──── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:15:08 UTC
#> # A tibble: 1 × 159
#>   game_state_key period hd         id season    stage_id type_id commentator
#>   <chr>           <int> <lgl>   <int> <chr>        <int>   <int> <lgl>      
#> 1 finished           -1 TRUE  2786714 2025/2026      370      18 TRUE       
#> # ℹ 151 more variables: likes_enabled <lgl>, is_vertical_video <lgl>,
#> #   khl_id <int>, arena_id <int>, arena_capacity <int>, arena_phone <chr>,
#> #   arena_website <chr>, arena_name <chr>, arena_address <chr>,
#> #   arena_city <chr>, arena_image <chr>, arena_geo_lat <dbl>,
#> #   arena_geo_long <dbl>, team_a_id <int>, team_a_khl_id <int>,
#> #   team_a_image <chr>, team_a_name <chr>, team_a_location <chr>,
#> #   team_a_shots <int>, team_a_gf <int>, team_a_ppg <int>, team_a_shg <int>, …
# }
```
