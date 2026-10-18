# **National League club record**

Club CMS record: names, logo, website, ticket links.

## Usage

``` r
nl_team_info(team_id, return_parsed = TRUE)
```

## Arguments

- team_id:

  SIHF team id (`team_id` in
  [`nl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nl_standings.md);
  the same id as the SIHF family's `team_id`), e.g. `101144` (EV Zug). A
  whole number or a digit string.

- return_parsed:

  Logical. `TRUE` (the default) returns the `fastRhockey_data` tibble;
  `FALSE` returns the raw JSON body as a nested list (api keys already
  dropped).

## Value

A `fastRhockey_data` tibble (27 columns) with the columns below, or the
raw JSON body when `return_parsed = FALSE`. A valid but empty payload
returns a zero-row tibble with these columns.

|  |  |  |
|----|----|----|
| col_name | type | description |
| name | character | Club name from the club's CMS record |
| cms_node_id | numeric | Umbraco CMS node id of the club page (whole number; not a team id). |
| url_segment | character | URL slug of the club page |
| type | character | CMS node kind ('Content') |
| content_type | character | CMS document type ('club') |
| url | character | Site path of the club page |
| fields_grid | character | Page layout blocks; unobserved: null or empty in every sampled row |
| fields_logo_name | character | Title of the club logo media item in the club CMS |
| fields_logo_id | numeric | Umbraco CMS media id; not a team id |
| fields_logo_url_segment | character | URL slug; unobserved: null or empty in every sampled row |
| fields_logo_type | character | CMS node kind ('Media') |
| fields_logo_content_type | character | Media kind ('Image') |
| fields_logo_url | character | Site path of the image |
| fields_logo_fields_umbraco_file_src | character | Site path of the file |
| fields_logo_fields_umbraco_file_focal_point | character | Crop focal point; unobserved: null or empty in every sampled row |
| fields_logo_fields_umbraco_file_crops | character | Crop presets; unobserved: null or empty in every sampled row |
| fields_logo_fields_umbraco_width | integer | Image width, pixels |
| fields_logo_fields_umbraco_height | integer | Image height, pixels |
| fields_logo_fields_umbraco_bytes | numeric | File size, bytes |
| fields_logo_fields_umbraco_extension | character | File extension of the club logo media file |
| team_id | numeric | SIHF team id (whole number; e.g. 101151 = HC Davos). The same key space as the sihf family's team_id. |
| fields_website | character | Club website URL |
| fields_ticket_url | character | Club ticket-shop URL |
| fields_game_ticket_url | character | Per-game ticket URL template; empty string in the sample |
| fields_overriden_name | character | Display-name override (sic); empty string in the sample |
| fields_players | character | Roster override; empty string in the sample |
| fields_api_name | character | Club key in the app's backend (e.g. 'evz') |

## Details

**robots.txt ruling.** `www.nationalleague.ch/robots.txt` disallows
`/api`. The owner approved building on it on 2026-10-12, for polite,
incremental use only: requests are spaced at least one second apart (on
every attempt, retries included), and a bulk backfill or a published
mirror needs the owner's sign-off first. Crawl incrementally: fetch a
finished game once (keyed by `game_id`), find new games with one
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md)
call per run, and fetch season tables at most once per game day.

Seasons are END years and go on the wire unchanged. Ids (`*_id`) are
whole-number `numeric` (a 14-digit `game_id` exceeds R's 32-bit
integer); every other whole-number column is `integer`. The club backend
key the API ships in `/teams/{id}/info` is dropped before parsing and
never returned.

Endpoint: `GET https://www.nationalleague.ch/api/teams/{teamId}/info`.

## Errors

Raises a classed condition (all inherit `fastRhockey_error`):
`fastRhockey_no_data` for a 404 or an empty answer (no games or rows for
that season, team or phase), `fastRhockey_asset_fetch` for a failed
fetch, an error object in a 200, a body without the ids it must carry,
or an answer for another season, team, game or player, and
`fastRhockey_bad_request` for a 400/422 or an argument that is not a
whole number.

## See also

Other National League Functions:
[`nl_current_games()`](https://fastRhockey.sportsdataverse.org/reference/nl_current_games.md),
[`nl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/nl_game_overview.md),
[`nl_lineups()`](https://fastRhockey.sportsdataverse.org/reference/nl_lineups.md),
[`nl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nl_pbp.md),
[`nl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_box.md),
[`nl_player_season()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_season.md),
[`nl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_player_stats.md),
[`nl_playoffs()`](https://fastRhockey.sportsdataverse.org/reference/nl_playoffs.md),
[`nl_playouts()`](https://fastRhockey.sportsdataverse.org/reference/nl_playouts.md),
[`nl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_schedule.md),
[`nl_shots()`](https://fastRhockey.sportsdataverse.org/reference/nl_shots.md),
[`nl_standings()`](https://fastRhockey.sportsdataverse.org/reference/nl_standings.md),
[`nl_team_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_player_stats.md),
[`nl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_schedule.md),
[`nl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/nl_team_stats.md),
[`nl_top_scorers()`](https://fastRhockey.sportsdataverse.org/reference/nl_top_scorers.md)

## Examples

``` r
# \donttest{
  try(nl_team_info(team_id = 101144))
#> ── National League Team Info Information from nationalleague.ch ────────────────
#> ℹ Data updated: 2026-10-18 06:17:25 UTC
#> # A tibble: 1 × 27
#>   name   cms_node_id url_segment type    content_type url            fields_grid
#>   <chr>        <dbl> <chr>       <chr>   <chr>        <chr>          <chr>      
#> 1 EV Zug        1627 ev-zug      Content club         /clubs/ev-zug/ []         
#> # ℹ 20 more variables: fields_logo_name <chr>, fields_logo_id <dbl>,
#> #   fields_logo_url_segment <chr>, fields_logo_type <chr>,
#> #   fields_logo_content_type <chr>, fields_logo_url <chr>,
#> #   fields_logo_fields_umbraco_file_src <chr>,
#> #   fields_logo_fields_umbraco_file_focal_point <chr>,
#> #   fields_logo_fields_umbraco_file_crops <chr>,
#> #   fields_logo_fields_umbraco_width <int>, …
# }
```
