# One team

Wraps `GET https://www.shl.se/api/sports-v2/teams/{team_uuid}` of the
SHL site API (www.shl.se). Unofficial use of a public source; seasons
are END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_team(team_uuid, return_parsed = TRUE)
```

## Arguments

- team_uuid:

  Team uuid (homeTeamInfo.uuid / awayTeamInfo.uuid in the schedule, or
  teamList\[\].uuid).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
| names_short | character | Short club name ('Herr' = men's team). |
| names_long | character | Long club name ('Herr' = men's team). |
| names_full | character | Full club name ('Herr' = men's team). |
| names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
| names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
| names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
| names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
| info_founded | character | Club founding year (text). Numeric value sent as a string. |
| info_golds | character | Club's championships (text). |
| info_finals | character | Club's final appearances (text). |
| info_retired_numbers | character | Retired jersey numbers (text). |
| info_address | character | Club postal address. |
| info_phone | character | Telephone number of the club. |
| info_fax | character | Fax number of the club. |
| info_email | character | Contact email address of the club. |
| info_chairman | character | Name of the club chairman. |
| info_manager | character | Club general manager. |
| info_accreditation | character | Press-accreditation contact, free Swedish text (e.g. 'Senast dagen före match till …' = 'no later than the day before the game, to …'). |
| info_facebook | character | Club Facebook URL. |
| info_instagram | character | Club Instagram URL. |
| info_twitter | character | Club X/Twitter URL. |
| contact | character | Club contact text. |
| uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
| nationality | character | Nationality (ISO country code). |
| arena_name | character | Name of the arena hosting the game. Empty string in every sample (placeholder). |
| public_url | character | Public image URL. |
| icon | character | URL of the team logo image. |
| owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
| site_display_name | character | Club name as shown on its own site (e.g. Frölunda HC). |

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
[`shl_athlete_details()`](https://fastRhockey.sportsdataverse.org/reference/shl_athlete_details.md),
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
[`shl_team_played_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_played_games.md),
[`shl_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_stats.md),
[`shl_team_upcoming_games()`](https://fastRhockey.sportsdataverse.org/reference/shl_team_upcoming_games.md)

## Examples

``` r
# \donttest{
  shl_team(team_uuid = "087a-087aTQv9u")
#> ── SHL Team Information from www.shl.se ────────────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:20:23 UTC
#> # A tibble: 1 × 30
#>   names_code names_short names_long  names_full names_code_site names_short_site
#>   <chr>      <chr>       <chr>       <chr>      <chr>           <chr>           
#> 1 FHC        "Frölunda " Frölunda HC Frölunda … FHC             Frölunda        
#> # ℹ 24 more variables: names_full_site <chr>, names_long_site <chr>,
#> #   info_founded <chr>, info_golds <chr>, info_finals <chr>,
#> #   info_retired_numbers <chr>, info_address <chr>, info_phone <chr>,
#> #   info_fax <chr>, info_email <chr>, info_chairman <chr>, info_manager <chr>,
#> #   info_accreditation <chr>, info_facebook <chr>, info_instagram <chr>,
#> #   info_twitter <chr>, contact <chr>, uuid <chr>, nationality <chr>,
#> #   arena_name <chr>, public_url <chr>, icon <chr>, owner_instance_id <chr>, …
# }
```
