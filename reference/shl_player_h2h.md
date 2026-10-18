# Player head-to-head for a matchup

Wraps
`GET https://www.shl.se/api/statistics-v2/player-h2h- v2/{ssgt_uuid}/{home_team_uuid}/{away_team_uuid}`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_player_h2h(
  ssgt_uuid,
  home_team_uuid,
  away_team_uuid,
  game_uuid = NULL,
  return_parsed = TRUE
)
```

## Arguments

- ssgt_uuid:

  ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular
  season). Directory: /statistics-v2/layout-info
  seasonSeriesGameTypeTeams\[\].ssgtUuid.

- home_team_uuid:

  Home team uuid of the game.

- away_team_uuid:

  Away team uuid of the game.

- game_uuid:

  Game uuid; narrows the head-to-head to that game's line-ups.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| uuid | character | Athlete uuid (platform key space; qQ9-... or 10-char form); joins to the athlete routes and stats-info stats\[\].info.uuid. Not the isa or ramses_legacy id. |
| first_name | character | Given (first) name of the player. |
| last_name | character | Surname (family name) of the player. |
| full_name | character | Given name and surname. |
| birth_date | character | Date of birth (YYYY-MM-DD). |
| nationality | character | Nationality (ISO country code). |
| gender | character | Sex of the athlete or team (male / female). |
| jersey_number | character | Jersey (shirt) number of the player. |
| position | character | Position label in English (e.g. Forward); N/A when unknown. |
| position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
| age_value | integer | Age of the player in whole years. |
| age_format | character | Unit of value: years. |
| height_value | integer | Height of the player in centimetres. |
| height_format | character | Unit of value: cm. |
| weight_value | integer | Weight of the player in kilograms. |
| weight_format | character | Unit of value: kg. |
| team_uuid | character | Platform team uuid. |
| team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
| team_name | character | Name of the team. |
| team_code | character | StatNet team code (SAIK, FHC). |
| team_media | character | Media URL or string. |
| media_url | character | Portrait URL on the imgix CDN. |
| media_alt | character | Image alt text. Empty string in every sample (placeholder). |
| media_srcset | character | Responsive image srcset. |
| season_stats | character | Current-season stat rows of the player (empty in the sample). |
| career_stats | character | Career stat rows of the player (empty in the sample). |
| match_stats_nr | integer | Jersey (shirt) number of the player. |
| match_stats_pos | character | Position code in this game (e.g. CE, LW, RW, D, GK). |
| match_stats_line | integer | Line (unit) number the player was listed on. |
| match_stats_g | integer | Number of goals scored. |
| match_stats_a | integer | Number of assists credited. |
| match_stats_tp | integer | Points (goals + assists). |
| match_stats_shg | integer | Shorthanded goals. |
| match_stats_ppg | integer | Power-play goals. |
| match_stats_sw | integer | Number of shots that missed the net. |
| match_stats_pim | integer | Penalty minutes. |
| match_stats_sog | integer | Number of shots on goal. |
| match_stats_ppsog | integer | Power-play shots on goal. |
| match_stats_nep | integer | On ice for goals against (count). |
| match_stats_pop | integer | On ice for goals for (count). |
| match_stats_toi | character | Time on ice (mm:ss). Clock/duration as an mm:ss string. |
| match_stats_hits | integer | Body checks delivered. |
| match_stats_fo_tot | integer | Face-offs taken. |
| match_stats_fow | integer | Number of face-offs won. |
| match_stats_fol | integer | Face-offs lost. |
| match_stats_num_pen | integer | Number of penalties. |
| match_stats_fo_perc | numeric | Face-off win %. |
| match_stats_ms | integer | Missed shots (wide + blocked). |
| match_stats_bk_s | integer | Number of blocked shots. |
| match_stats_plus_minus | integer | Goals for minus goals against while on ice (+/-). |
| season_avg_stats | character | Head-to-head season averages. |
| statistics_provider | character | Stats provider of the data (statnet seen). |
| is_in_squad | logical | Whether the player is in the current squad. |
| effective_ssgt_uuid | character | The ssgt the head-to-head was computed for: the CURRENT season, whatever the path asked. |

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
  shl_player_h2h(ssgt_uuid = "iuzqg7dqk9", home_team_uuid = "087a-087aTQv9u",
    away_team_uuid = "41c4-41c4BiYZU", game_uuid = "bdhvuc5tex")
#> ── SHL Player H2h Information from www.shl.se ──────── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:18:10 UTC
#> # A tibble: 46 × 60
#>    uuid          first_name last_name    full_name birth_date nationality gender
#>    <chr>         <chr>      <chr>        <chr>     <chr>      <chr>       <chr> 
#>  1 qQ9-9d31rLEXi Max        Friberg      Max Frib… 1992-11-20 SE          "male"
#>  2 qZn-BWlb8Aoj  Ty         Rattie       Ty Rattie 1993-02-05 CA          "male"
#>  3 qcw-3QbUIyuRn Arttu      Ruotsalainen Arttu Ru… 1997-10-29 FI          ""    
#>  4 qRe-glixIcxZ  Fredrik    Karlström    Fredrik … 1998-01-12 SE          "male"
#>  5 qV0-1dPP3DJH6 Oscar      Lawner       Oscar La… 2001-02-13 SE          "male"
#>  6 qRo-4uyOhblgh Erik       Norén        Erik Nor… 2001-10-11 SE          "male"
#>  7 qee-2LDnbExBt Dominik    Egli         Dominik … 1998-08-20 CH          ""    
#>  8 qWb-2harGUCbA Jonathan   Myrenberg    Jonathan… 2003-04-11 SE          "male"
#>  9 qQ9-349c7WDv6 Lars       Johansson    Lars Joh… 1987-07-11 SE          "male"
#> 10 qQ9-d14fFrVkN Nicklas    Lasu         Nicklas … 1989-09-16 SE          "male"
#> # ℹ 36 more rows
#> # ℹ 53 more variables: jersey_number <chr>, position <chr>,
#> #   position_code <chr>, age_value <int>, age_format <chr>, height_value <int>,
#> #   height_format <chr>, weight_value <int>, weight_format <chr>,
#> #   team_uuid <chr>, team_instance_id <chr>, team_name <chr>, team_code <chr>,
#> #   team_media <chr>, media_url <chr>, media_alt <chr>, media_srcset <chr>,
#> #   season_stats <chr>, career_stats <chr>, match_stats_nr <int>, …
# }
```
