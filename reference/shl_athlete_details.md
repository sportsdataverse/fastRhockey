# Player bio plus playerExtIds (isa, ramses_legacy, ...)

Wraps
`GET https://www.shl.se/api/sports-v2/athlete-details/{athlete_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_athlete_details(athlete_uuid, return_parsed = TRUE)
```

## Arguments

- athlete_uuid:

  Athlete uuid (stats-info stats\[\].info.uuid, roster rows, or
  athlete-details).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| athlete_data_uuid | character | Athlete uuid (platform key space; qQ9-... or 10-char form); joins to the athlete routes and stats-info stats\[\].info.uuid. Not the isa or ramses_legacy id. |
| athlete_data_contact_info | character | Contact info (null in captures). |
| athlete_data_created_at | character | Record creation time (ISO 8601; often null). |
| athlete_data_created_by | character | Record creator (site instance id; often null). |
| athlete_data_creator_site | character | Site that created the record (null in captures). |
| athlete_data_height | integer | Height of the player in centimetres. |
| athlete_data_date_of_birth | character | Date of birth (YYYY-MM-DD). |
| athlete_data_deleted | logical | Soft-delete flag of the record (false in every sample). |
| athlete_data_first_name | character | Given (first) name of the player. |
| athlete_data_gender | character | Sex of the athlete or team (male / female). |
| athlete_data_last_modified | character | Record last-modified time (ISO 8601). |
| athlete_data_last_name | character | Surname (family name) of the player. |
| athlete_data_metadata | character | Free-form metadata. |
| athlete_data_nationality | character | Nationality (ISO country code). |
| athlete_data_shoots | character | Shooting side (L / R; Left / Right). |
| athlete_data_weight | integer | Weight of the player in kilograms. |
| athlete_data_player_ext_ids | character | External ids of the athlete (isa, ramses_legacy, ramses_generic_legacy, ramses_merged). |

## Errors

`fastRhockey_no_data`: the host answered 404, an empty body (gameday
routes before 2023/24), a table with no rows, or the season and game
type are not in layout-info. `fastRhockey_asset_fetch`: any other
non-2xx answer or a dead connection after retries, a non-JSON or
error-envelope 200, a stats-info page that reaches `count` or falls
short of its `totalCount`, or a body for another game, team, athlete or
ssgt than the one asked for. `fastRhockey_bad_request`: a 400 / 422, a
season that is not a year, or an argument outside its choices.

## See also

Other SHL Functions:
[`shl_athlete_career()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_career.md),
[`shl_athlete_game_log()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_game_log.md),
[`shl_athlete_profile()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_profile.md),
[`shl_athlete_seasons()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_seasons.md),
[`shl_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/shl_boxscore.md),
[`shl_game_header()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_header.md),
[`shl_game_overview()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_overview.md),
[`shl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/shl_game_summary.md),
[`shl_last_game_day()`](https://fastRhockey.sportsdataverse.org/reference/shl_last_game_day.md),
[`shl_layout_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_layout_info.md),
[`shl_next_game_day()`](https://fastRhockey.sportsdataverse.org/reference/shl_next_game_day.md),
[`shl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/shl_pbp.md),
[`shl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_box.md),
[`shl_player_crosswalk()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_crosswalk.md),
[`shl_player_h2h()`](https://fastRhockey.sportsdataverse.org/reference/shl_player_h2h.md),
[`shl_post_game_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_post_game_team_stats.md),
[`shl_roster()`](https://fastRhockey.sportsdataverse.org/reference/shl_roster.md),
[`shl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/shl_schedule.md),
[`shl_season_filter()`](https://fastRhockey.sportsdataverse.org/reference/shl_season_filter.md),
[`shl_sites()`](https://fastRhockey.sportsdataverse.org/reference/shl_sites.md),
[`shl_ssgt_provider()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_provider.md),
[`shl_ssgt_uuid()`](https://fastRhockey.sportsdataverse.org/reference/shl_ssgt_uuid.md),
[`shl_standings()`](https://fastRhockey.sportsdataverse.org/reference/shl_standings.md),
[`shl_stats_info()`](https://fastRhockey.sportsdataverse.org/reference/shl_stats_info.md),
[`shl_team()`](https://fastRhockey.sportsdataverse.org/reference/shl_team.md),
[`shl_team_played_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_played_games.md),
[`shl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_stats.md),
[`shl_team_upcoming_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_upcoming_games.md)

## Examples

``` r
# \donttest{
  shl_athlete_details(athlete_uuid = "qQ9-349c7WDv6")
#> ── SHL Athlete Details Information from www.shl.se ─── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:19:57 UTC
#> # A tibble: 1 × 17
#>   athlete_data_uuid athlete_data_contact_info athlete_data_created_at 
#>   <chr>             <chr>                     <chr>                   
#> 1 qQ9-349c7WDv6     NA                        2024-06-24T07:49:36.000Z
#> # ℹ 14 more variables: athlete_data_created_by <chr>,
#> #   athlete_data_creator_site <chr>, athlete_data_height <int>,
#> #   athlete_data_date_of_birth <chr>, athlete_data_deleted <lgl>,
#> #   athlete_data_first_name <chr>, athlete_data_gender <chr>,
#> #   athlete_data_last_modified <chr>, athlete_data_last_name <chr>,
#> #   athlete_data_metadata <chr>, athlete_data_nationality <chr>,
#> #   athlete_data_shoots <chr>, athlete_data_weight <int>, …
# }
```
