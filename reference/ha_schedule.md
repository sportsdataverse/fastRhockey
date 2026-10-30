# **HockeyAllsvenskan Schedule (season in progress)**

The current season's games (CMS records, decoded from the schedule
page): slug, StatNet game number and team ids, start time, score.

The `slug`, `statnet_game_number`, `home_statnet_id`, `away_statnet_id`
and `scheduled_date_time` columns are the
[`ha_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ha_pbp.md)
request. The page lists the season in progress only; `season` is the END
year (`cms_season` `season_2026` = 2026/27 is `season` 2027). Ids:
`home_statnet_id` / `away_statnet_id` are StatNet team ids,
`home_team_code` / `away_team_code` display codes, `game_cms_id` and
`home_team_cms_id` / `away_team_cms_id` CMS ids (three team key spaces,
kept in separate columns). Scores and attendance are sent as strings and
parsed to integers (an attendance of 0 for a played game means unknown).

## Usage

``` r
ha_schedule(return_parsed = TRUE)
```

## Arguments

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (stamped with the request under `_request` where
  the body does not say what it answers).

## Value

A tibble (`return_parsed = TRUE`), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season END year (2026 = 2025/26). |
| game_cms_id | integer | CMS (Strapi) record id of the game. |
| game_document_id | character | CMS (Strapi) document id of the game. |
| cms_season | character | CMS season key as sent (season_2026 = 2026/27: a START year). |
| statnet_game_number | integer | StatNet game number (23401 = 2026/27 game 1); the play-by-play request key. |
| slug | character | Game slug yyyymmdd-home-away (lower-case CMS short names); the /api/game?slug= key. |
| round | integer | Round number of the game in the season schedule. |
| scheduled_date_time | character | Scheduled face-off time (ISO 8601 UTC). |
| venue | character | Arena name on the CMS game record. |
| attendance | integer | Attendance; null for an unplayed game (0 for a played one means unknown). |
| current_period | character | Live period while the game runs; empty string ” before and after. |
| current_period_start | character | Start time of the live period; null outside a live game. |
| home_score | integer | Home goals, final. |
| away_score | integer | Away goals, final. |
| ended_date_time | character | End time (ISO 8601 UTC). |
| decided_in | character | How the game was decided (FT, OT, SO); empty for a game not yet played. |
| home_score_p1 | integer | Home goals in period 1. |
| away_score_p1 | integer | Away goals in period 1. |
| home_score_p2 | integer | Home goals in period 2. |
| away_score_p2 | integer | Away goals in period 2. |
| home_score_p3 | integer | Home goals in period 3. |
| away_score_p3 | integer | Away goals in period 3. |
| home_ot_score | integer | Home overtime goals; null when the game had no overtime. |
| away_ot_score | integer | Away overtime goals; null when the game had no overtime. |
| home_so_score | integer | Home shootout goals; null when the game had no shootout. |
| away_so_score | integer | Away shootout goals; null when the game had no shootout. |
| game_type | character | Game type (phase): HA regular season, HAPlayoff playoffs, HAkval playout. |
| play_off_game | character | Game of the playoff series (1, 2, ...); null outside the playoffs. |
| play_off_game_level | character | Playoff round label. |
| played_date_time | character | Actual start time (ISO 8601 UTC). |
| is_completed | logical | true once the game is final. |
| home_statnet_id | character | StatNet team id of the home team (AIK, MODO, MIK). |
| away_statnet_id | character | StatNet team id of the away team. |
| created_at | character | CMS creation time of the record (ISO 8601 UTC). |
| updated_at | character | CMS update time of the record (ISO 8601 UTC). |
| published_at | character | CMS publish time of the record (ISO 8601 UTC). |
| home_team_cms_id | integer | CMS (Strapi) record id of the home team (81 = AIK). |
| home_team_document_id | character | Home team: CMS (Strapi) document id (string; stable across drafts and locales). |
| home_team_name | character | Home team: short club name (the teams list's name). |
| home_team_code | character | Display code of the home team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| home_team_main_color_hex | character | Home team: primary colour (hex). |
| home_team_alternate_color_hex | character | Home team: secondary colour (hex). |
| home_team_team_arena | character | Home team: home arena. |
| home_team_arena_address | character | Home team: postal address of the arena (multi-line, newline-separated). |
| home_team_arena_latitude | numeric | Home team: arena latitude, decimal degrees. |
| home_team_arena_longitude | numeric | Home team: arena longitude, decimal degrees. |
| home_team_arena_telephone | character | Home team: arena telephone contact. |
| home_team_tickets_provider_url | character | Home team: ticket shop URL. |
| home_team_club_mate_lottery_url | character | Home team: club lottery URL. |
| home_team_created_at | character | Home team: CMS creation time of the record (ISO 8601 UTC). |
| home_team_updated_at | character | Home team: CMS update time of the record (ISO 8601 UTC). |
| home_team_published_at | character | Home team: CMS publish time of the record (ISO 8601 UTC). |
| home_team_logo_url | character | Logo URL of the home team. |
| away_team_cms_id | integer | CMS (Strapi) record id of the away team. |
| away_team_document_id | character | Away team: CMS (Strapi) document id (string; stable across drafts and locales). |
| away_team_name | character | Away team: short club name (the teams list's name). |
| away_team_code | character | Display code of the away team, not the StatNet id. |
| away_team_main_color_hex | character | Away team: primary colour (hex). |
| away_team_alternate_color_hex | character | Away team: secondary colour (hex). |
| away_team_team_arena | character | Away team: home arena. |
| away_team_arena_address | character | Away team: postal address of the arena (multi-line, newline-separated). |
| away_team_arena_latitude | numeric | Away team: arena latitude, decimal degrees. |
| away_team_arena_longitude | numeric | Away team: arena longitude, decimal degrees. |
| away_team_arena_telephone | character | Away team: arena telephone contact. |
| away_team_tickets_provider_url | character | Away team: ticket shop URL. |
| away_team_club_mate_lottery_url | character | Away team: club lottery URL. |
| away_team_created_at | character | Away team: CMS creation time of the record (ISO 8601 UTC). |
| away_team_updated_at | character | Away team: CMS update time of the record (ISO 8601 UTC). |
| away_team_published_at | character | Away team: CMS publish time of the record (ISO 8601 UTC). |
| away_team_logo_url | character | Logo URL of the away team. |

## Errors

`fastRhockey_no_data`: the site has nothing for the request (a season
before 2005/06, an unknown or pre-2025/26 game, a phase not played, a
body without rows). `fastRhockey_bad_request`: a value outside its
enumerated set, a field the site requires is missing, or HTTP 400/422.
`fastRhockey_asset_fetch`: any other non-2xx, a connection failure after
retries, or an empty, non-JSON, error-shaped, truncated or out-of-scope
200 (the answer is unknown, not empty).

## See also

Other HA Functions:
[`ha_game()`](https://fastRhockey.sportsdataverse.org/reference/ha_game.md),
[`ha_games()`](https://fastRhockey.sportsdataverse.org/reference/ha_games.md),
[`ha_goalie_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_goalie_stats.md),
[`ha_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ha_leaders.md),
[`ha_pbp()`](https://fastRhockey.sportsdataverse.org/reference/ha_pbp.md),
[`ha_players()`](https://fastRhockey.sportsdataverse.org/reference/ha_players.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_schedule())
#> ── HA Schedule Information from hockeyallsvenskan.se ───────────────────────────
#> ℹ Data updated: 2026-10-30 00:12:18 UTC
#> # A tibble: 364 × 70
#>    season game_cms_id game_document_id      cms_season statnet_game_number slug 
#>     <int>       <int> <chr>                 <chr>                    <int> <chr>
#>  1   2027        6566 sftwwgo7jg0avrwgpgt7… season_20…               23401 2026…
#>  2   2027        6567 s8un03yii0ql35tu2keu… season_20…               23402 2026…
#>  3   2027        6568 e8s75j95izbw35pz5il2… season_20…               23403 2026…
#>  4   2027        6569 bxnwszpk7n1hya74tdo1… season_20…               23404 2026…
#>  5   2027        6570 rhetw10d411ydt7m2etg… season_20…               23405 2026…
#>  6   2027        6571 xlxhq1q2g1vklg77oa9d… season_20…               23406 2026…
#>  7   2027        6572 gz1qhskov2fnowqgpcmv… season_20…               23407 2026…
#>  8   2027        7638 tnginq80ldvnemcpw3gd… season_20…               23408 2026…
#>  9   2027        7639 vls238ye8zl9jbij85s6… season_20…               23409 2026…
#> 10   2027        7640 q04k2qjt9hvekq51j3cw… season_20…               23410 2026…
#> # ℹ 354 more rows
#> # ℹ 64 more variables: round <int>, scheduled_date_time <chr>, venue <chr>,
#> #   attendance <int>, current_period <chr>, current_period_start <chr>,
#> #   home_score <int>, away_score <int>, ended_date_time <chr>,
#> #   decided_in <chr>, home_score_p1 <int>, away_score_p1 <int>,
#> #   home_score_p2 <int>, away_score_p2 <int>, home_score_p3 <int>,
#> #   away_score_p3 <int>, home_ot_score <int>, away_ot_score <int>, …
# }
```
