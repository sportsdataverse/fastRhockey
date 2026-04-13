# Changelog

## **fastRhockey 1.0.0 (continued development)**

#### **PWHL parity: 3 new NHL loaders + datasets**

Three new season-level NHL loaders that bring NHL coverage in line with
PWHL. Each is backed by a new release tag on
[`sportsdataverse/sportsdataverse-data`](https://github.com/sportsdataverse/sportsdataverse-data)
and powered by new extractors in
`fastRhockey-nhl-raw/R/scrape_nhl_raw.R` and
`fastRhockey-nhl-data/R/nhl_data_creation.R`.

| function                                                                                                      | release tag           | rows per game                |
|---------------------------------------------------------------------------------------------------------------|-----------------------|------------------------------|
| [`load_nhl_officials()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_officials.md)             | `nhl_officials`       | one per official (refs+lins) |
| [`load_nhl_shots_by_period()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shots_by_period.md) | `nhl_shots_by_period` | one per team per period      |
| [`load_nhl_shootout()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shootout.md)               | `nhl_shootout`        | one per shootout attempt     |

#### **`nhl_schedule()` data-availability flags**

[`nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule.md)
gained an `include_data_flags = FALSE` parameter. When `TRUE`, the live
schedule is left-joined against the cached `nhl_games_in_data_repo`
index from the data repo and gains 16 logical columns (`PBP`,
`team_box`, `player_box`, `skater_box`, `goalie_box`, `game_info`,
`game_rosters`, `scoring`, `penalties`, `scratches`, `linescore`,
`three_stars`, `shifts`, `officials`, `shots_by_period`, `shootout`)
telling you which pre-compiled datasets cover each game. The schedule
files in `fastRhockey-nhl-raw/nhl/schedules/` and the master schedule on
the data repo carry these same flags.

#### **Richer per-game JSON in `fastRhockey-nhl-raw`**

`scrape_nhl_raw.R` now writes four additional top-level keys into both
`nhl/json/raw/{game_id}.json` and `nhl/json/final/{game_id}.json`:

- `officials` — list of `{role, name}` (referees first, linesmen second)
- `shots_by_period` — long-form per-team-per-period shot totals
- `shootout` — per-attempt summary (NULL if game didn’t reach SO)
- `plays_by_period` — OLD-format index of `play_indices` per period
- `on_ice` / `on_ice_plus` / `penalty_box` (final-state reconstruction,
  derived from the processed PBP) — populated only in the `final/`
  variant since they need fastRhockey-enriched on-ice columns

#### **Refactor: NHL loaders consolidated into `R/nhl_loaders.R`**

All NHL season-level loaders and the shared `.nhl_release_loader()`
helper now live in `R/nhl_loaders.R`, mirroring the PWHL convention
(`R/pwhl_loaders.R`). The empty `R/nhl_pbp.R` file was removed since it
never contained an actual `nhl_pbp()` scraper — only loaders. Public
signatures and return types are unchanged.

#### **New NHL Edge Analytics (33 functions)**

Wraps the NHL Edge advanced-metrics endpoints under
`https://api-web.nhle.com/v1/edge/...`. Every wrapper accepts an
optional `season` argument (4-digit end year, e.g., `2025`) — when
omitted, the `/now` form is used to fetch the current season. All
wrappers share an internal `.nhl_edge_api()` helper in
`R/helpers_nhl_edge.R`.

| family | functions                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
|--------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Skater | [`nhl_edge_skater_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_detail.md), [`nhl_edge_skater_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_landing.md), [`nhl_edge_skater_comparison()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_comparison.md), [`nhl_edge_skater_shot_location_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_location_detail.md), [`nhl_edge_skater_shot_location_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_location_top_10.md), [`nhl_edge_skater_shot_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_speed_detail.md), [`nhl_edge_skater_shot_speed_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_shot_speed_top_10.md), [`nhl_edge_skater_skating_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_skating_speed_detail.md), [`nhl_edge_skater_speed_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_speed_top_10.md), [`nhl_edge_skater_skating_distance_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_skating_distance_detail.md), [`nhl_edge_skater_distance_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_distance_top_10.md), [`nhl_edge_skater_zone_time()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_zone_time.md), [`nhl_edge_skater_zone_time_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_skater_zone_time_top_10.md), [`nhl_cat_edge_skater_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_cat_edge_skater_detail.md) |
| Goalie | [`nhl_edge_goalie_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_detail.md), [`nhl_edge_goalie_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_landing.md), [`nhl_edge_goalie_comparison()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_comparison.md), [`nhl_edge_goalie_5v5_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_5v5_detail.md), [`nhl_edge_goalie_5v5_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_5v5_top_10.md), [`nhl_edge_goalie_save_percentage_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_save_percentage_detail.md), [`nhl_edge_goalie_edge_save_pctg_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_edge_save_pctg_top_10.md), [`nhl_edge_goalie_shot_location_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_shot_location_detail.md), [`nhl_edge_goalie_shot_location_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_goalie_shot_location_top_10.md), [`nhl_cat_edge_goalie_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_cat_edge_goalie_detail.md)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| Team   | [`nhl_edge_team_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_detail.md), [`nhl_edge_team_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_landing.md), [`nhl_edge_team_shot_location_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_shot_location_detail.md), [`nhl_edge_team_shot_location_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_shot_location_top_10.md), [`nhl_edge_team_shot_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_shot_speed_detail.md), [`nhl_edge_team_skating_speed_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_speed_detail.md), [`nhl_edge_team_skating_speed_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_speed_top_10.md), [`nhl_edge_team_skating_distance_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_distance_detail.md), [`nhl_edge_team_skating_distance_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_skating_distance_top_10.md), [`nhl_edge_team_zone_time_details()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_zone_time_details.md), [`nhl_edge_team_zone_time_top_10()`](https://fastRhockey.sportsdataverse.org/reference/nhl_edge_team_zone_time_top_10.md)                                                                                                                                                                                                                                                                                                                                                                    |

#### **New NHL Records API integration (25 functions)**

First-time integration with `https://records.nhl.com/site/api/`. All
wrappers share an internal `.nhl_records_api()` helper in
`R/helpers_nhl_records.R` that supports `cayenneExp` filters,
`limit`/`start` pagination, and the records-API response shape
(`{data, total}`).

- Franchise:
  [`nhl_records_franchise()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise.md),
  [`nhl_records_franchise_detail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_detail.md),
  [`nhl_records_franchise_totals()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_totals.md),
  [`nhl_records_franchise_team_totals()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_team_totals.md),
  [`nhl_records_franchise_season_results()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_season_results.md),
  [`nhl_records_franchise_playoff_appearances()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_franchise_playoff_appearances.md)
- Player:
  [`nhl_records_player()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_player.md),
  [`nhl_records_player_byteam()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_player_byteam.md),
  [`nhl_records_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_player_stats.md),
  [`nhl_records_skater_real_time_stats_season()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_skater_real_time_stats_season.md),
  [`nhl_records_skater_real_time_stats_career()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_skater_real_time_stats_career.md)
- Goalie:
  [`nhl_records_goalie_career_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_goalie_career_stats.md),
  [`nhl_records_goalie_season_stats()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_goalie_season_stats.md),
  [`nhl_records_goalie_shutout_streak()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_goalie_shutout_streak.md)
- Draft:
  [`nhl_records_draft()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft.md),
  [`nhl_records_draft_lottery_odds()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft_lottery_odds.md),
  [`nhl_records_draft_lottery_picks()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft_lottery_picks.md),
  [`nhl_records_draft_prospect()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_draft_prospect.md)
- Awards / HOF:
  [`nhl_records_trophy()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_trophy.md),
  [`nhl_records_award_details()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_award_details.md),
  [`nhl_records_hof_players()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_hof_players.md)
- League:
  [`nhl_records_officials()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_officials.md),
  [`nhl_records_attendance()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_attendance.md),
  [`nhl_records_venue()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_venue.md),
  [`nhl_records_combine()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_combine.md)

#### **New NHL Stats REST wrappers (13 functions)**

Promotes endpoints that were previously only reachable via
[`nhl_stats_misc()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_misc.md)’s
generic dispatcher into discoverable, dedicated wrappers. Documentation
for
[`nhl_stats_misc()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_misc.md)
was also updated to enumerate every valid `endpoint` value.

- [`nhl_stats_franchise()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_franchise.md),
  [`nhl_stats_players()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_players.md),
  [`nhl_stats_glossary()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_glossary.md),
  [`nhl_stats_country()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_country.md),
  [`nhl_stats_config()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_config.md),
  [`nhl_stats_ping()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_ping.md)
- [`nhl_stats_skater_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_leaders.md),
  [`nhl_stats_goalie_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_leaders.md),
  [`nhl_stats_skater_milestones()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_milestones.md),
  [`nhl_stats_goalie_milestones()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_milestones.md)
- [`nhl_stats_team_listing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_team_listing.md),
  [`nhl_stats_game_listing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_game_listing.md),
  [`nhl_stats_content_module()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_content_module.md)

#### **New NHL api-web miscellaneous endpoints (6 functions + 3 in-place updates)**

- `nhl_wsc_pbp(game_id)` – narrative-format play-by-play from
  `wsc/play-by-play/{gameId}` (distinct from
  [`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
  which uses `gamecenter/{id}/play-by-play`).
- [`nhl_draft_tracker()`](https://fastRhockey.sportsdataverse.org/reference/nhl_draft_tracker.md)
  – live draft tracker (`draft-tracker/picks/now`), distinct from
  [`nhl_draft()`](https://fastRhockey.sportsdataverse.org/reference/nhl_draft.md)
  which hits the static `draft/picks/now` endpoint.
- `nhl_ppt_replay(game_id, event_number)` – event-level replay metadata.
- `nhl_ppt_replay_goal(game_id, event_number)` – goal-specific replay
  metadata.
- `nhl_postal_lookup(postal_code)` – broadcast region lookup by postal
  code.
- `nhl_smartlinks(handle = NULL)` – NHL.com smart-link router.
- `nhl_scoreboard(date = NULL)` – now accepts an optional `date`
  argument so callers can fetch historical scoreboards (was hardcoded to
  `/now`).
- `nhl_meta(game_id = NULL, year = NULL, series_letter = NULL)` – added
  a third branch for `meta/playoff-series/{year}/{seriesLetter}`.
- `nhl_draft_year(year, round = NULL)` – when `round` is `NULL` or
  `"all"`, the function now hits the `/draft/picks/{year}/all` shortcut
  in a single request instead of looping per round.

#### **New helper aggregators inspired by `nhl-api-py` (6 functions)**

Convenience wrappers that orchestrate multiple endpoint calls into one
tidy data frame:

- `nhl_game_ids_by_season(season, game_types, team_abbr, sleep_rate)` –
  iterates every team’s season schedule and returns the deduplicated set
  of game IDs.
- `nhl_all_players_by_season(season, sleep_rate)` – iterates every
  team’s roster and flattens forwards/defensemen/goalies.
- `nhl_player_career_stats(player_id)` – combines `player/{id}/landing`
  with the season-by-season `seasonTotals` payload into a career frame.
- `nhl_team_summary_range(start_season, end_season)` – multi-season team
  summary loop over `nhl_stats_teams(report_type = "summary")`.
- `nhl_skater_summary_range(start_season, end_season)` – same for skater
  summary.
- `nhl_goalie_summary_range(start_season, end_season)` – same for goalie
  summary.

#### **Endpoint mapping + OpenAPI specs**

- `data-raw/nhl_missing_endpoint_function_mapping.md` – table of every
  documented NHL API endpoint and its proposed/implemented fastRhockey
  wrapper, sourced from
  [dfleis/nhl-api-docs](https://github.com/dfleis/nhl-api-docs),
  [RentoSaijo/nhlscraper](https://github.com/RentoSaijo/nhlscraper), and
  [coreyjs/nhl-api-py](https://github.com/coreyjs/nhl-api-py).
- `data-raw/nhl_api_web_openapi.{json,yaml}` – OpenAPI 3.0.3 spec for
  `api-web.nhle.com/v1/` (132 endpoints).
- `data-raw/nhl_stats_rest_openapi.{json,yaml}` – OpenAPI 3.0.3 spec for
  `api.nhle.com/stats/rest/{lang}/` (21 endpoints).
- `data-raw/nhl_records_openapi.{json,yaml}` – OpenAPI 3.0.3 spec for
  `records.nhl.com/site/api/` (442 endpoints).
- `data-raw/_gen_openapi.py` – regenerator script.

#### **Bug Fixes / API quirks documented**

- [`nhl_stats_players()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_players.md)
  now detects `data: []` empty-list responses (the API returns this when
  no `cayenne_exp` filter is supplied) and returns `NULL` with a
  friendly hint instead of crashing `clean_names()`.
- [`nhl_stats_skater_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skater_leaders.md)
  and
  [`nhl_stats_goalie_leaders()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalie_leaders.md)
  no longer pass `start`/`limit` query parameters, which the
  `leaders/{skaters,goalies}/{attribute}` endpoint rejects with a 500
  Cayenne SQL error. Roxygen now documents the valid `attribute` values:
  skaters accept `assists`/`goals`/`points`; goalies accept
  `savePctg`/`gaa`/`shutouts`.
- `nhl_records_franchise(franchise_id = ...)` now translates the
  `franchise_id` argument into a `cayenneExp=id={id}` filter. The
  records API does not support a path-suffix `franchise/{id}` form
  (returns 404).
- [`nhl_records_award_details()`](https://fastRhockey.sportsdataverse.org/reference/nhl_records_award_details.md)
  – replaced the broken `franchise_id` argument with `season_id` (the
  `award-details` endpoint accepts `seasonId` filtering but rejects
  `franchiseId`).
- [`nhl_stats_content_module()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_content_module.md)
  now guards against unnamed CMS responses that previously crashed
  `clean_names()`.

#### **New NHL Loaders**

Ten new season-level loaders that pull pre-compiled NHL datasets from
new release tags on
[`sportsdataverse/sportsdataverse-data`](https://github.com/sportsdataverse/sportsdataverse-data).
All accept a `seasons` vector (Min: `2011`) and return a
`fastRhockey_data` data frame, mirroring the existing `load_nhl_*()`
API:

| function                                                                                                | release tag            | rows per game            |
|---------------------------------------------------------------------------------------------------------|------------------------|--------------------------|
| [`load_nhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_skater_box.md)     | `nhl_skater_boxscores` | one per skater           |
| [`load_nhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_goalie_box.md)     | `nhl_goalie_boxscores` | one per goalie           |
| [`load_nhl_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_game_rosters.md) | `nhl_game_rosters`     | one per dressed player   |
| [`load_nhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_game_info.md)       | `nhl_game_info`        | one                      |
| [`load_nhl_scoring()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_scoring.md)           | `nhl_scoring`          | one per goal             |
| [`load_nhl_penalties()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_penalties.md)       | `nhl_penalties`        | one per penalty          |
| [`load_nhl_three_stars()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_three_stars.md)   | `nhl_three_stars`      | up to three              |
| [`load_nhl_scratches()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_scratches.md)       | `nhl_scratches`        | one per scratched player |
| [`load_nhl_linescore()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_linescore.md)       | `nhl_linescore`        | one                      |
| [`load_nhl_shifts()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_shifts.md)             | `nhl_shifts`           | one per shift            |

The six pre-existing loaders
([`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md),
[`load_nhl_pbp_lite()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp_lite.md),
[`load_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_player_box.md),
[`load_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_team_box.md),
[`load_nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_schedule.md),
[`load_nhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_rosters.md))
were refactored to share a single internal worker
(`.nhl_release_loader()`) without changing their public signatures or
return types.

#### **New PWHL Loaders**

Eleven new season-level loaders that pull pre-compiled PWHL datasets
from new release tags on
[`sportsdataverse/sportsdataverse-data`](https://github.com/sportsdataverse/sportsdataverse-data).
All accept a `seasons` vector (Min: `2024`) and return a
`fastRhockey_data` data frame, mirroring the existing `load_pwhl_*()`
API:

| function                                                                                                        | release tag             | rows per game          |
|-----------------------------------------------------------------------------------------------------------------|-------------------------|------------------------|
| [`load_pwhl_skater_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_skater_box.md)           | `pwhl_skater_boxscores` | one per skater         |
| [`load_pwhl_goalie_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_goalie_box.md)           | `pwhl_goalie_boxscores` | one per goalie         |
| [`load_pwhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_team_box.md)               | `pwhl_team_boxscores`   | two (home/away)        |
| [`load_pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_game_info.md)             | `pwhl_game_info`        | one                    |
| [`load_pwhl_scoring_summary()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_scoring_summary.md) | `pwhl_scoring_summary`  | one per goal           |
| [`load_pwhl_penalty_summary()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_penalty_summary.md) | `pwhl_penalty_summary`  | one per penalty        |
| [`load_pwhl_three_stars()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_three_stars.md)         | `pwhl_three_stars`      | up to three            |
| [`load_pwhl_officials()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_officials.md)             | `pwhl_officials`        | one per official       |
| [`load_pwhl_shots_by_period()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shots_by_period.md) | `pwhl_shots_by_period`  | one per period         |
| [`load_pwhl_shootout()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_shootout.md)               | `pwhl_shootout`         | one per attempt        |
| [`load_pwhl_game_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_game_rosters.md)       | `pwhl_game_rosters`     | one per dressed player |

The four pre-existing loaders
([`load_pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_pbp.md),
[`load_pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_box.md),
[`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md),
[`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md))
were refactored to share a single internal worker
(`.pwhl_release_loader()`) without changing their public signatures or
return types.

## **fastRhockey 1.0.0**

#### **Breaking Changes**

- **Version 1.0.0** signals API stability for NHL and PWHL endpoints.
- All PHF functions are formally deprecated via `lifecycle`. The Premier
  Hockey Federation ceased operations; use PWHL functions instead.
  Functions will be removed in a future release.
- Consolidated new NHL API functions into existing NHL function names
  rather than creating `_v2` variants, since the original API endpoints
  were deprecated by the NHL. This means
  [`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md),
  [`nhl_game_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_boxscore.md),
  [`nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule.md),
  [`nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md),
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md),
  and
  [`nhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_info.md)
  now use the new `api-web.nhle.com` endpoints directly.

#### **New PWHL Functions**

- [`pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_pbp.md)
  – PWHL play-by-play data.
- [`pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_box.md)
  – PWHL player box scores (skaters and goalies).
- [`pwhl_game_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_info.md)
  – PWHL game information and metadata.
- [`pwhl_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_game_summary.md)
  – Detailed game summary (scoring, penalties, shots by period, three
  stars).
- [`pwhl_standings()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_standings.md)
  – PWHL league standings.
- [`pwhl_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_stats.md)
  – PWHL stat leaders (skaters and goalies).
- [`pwhl_leaders()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_leaders.md)
  – League leaders (top scorers and top goalies).
- [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md)
  – PWHL season ID lookup (now API-driven with fallback).
- [`pwhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_info.md)
  – Player biographical and profile information.
- [`pwhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_game_log.md)
  – Per-game statistics for a player in a season.
- [`pwhl_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_stats.md)
  – Career and season-by-season statistics for a player.
- [`pwhl_player_search()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_player_search.md)
  – Search for players by name.
- [`pwhl_transactions()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_transactions.md)
  – Player transactions for a season.
- [`pwhl_streaks()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_streaks.md)
  – Player streak data for a season.
- [`pwhl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_playoff_bracket.md)
  – Playoff bracket / series data.
- [`pwhl_scorebar()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_scorebar.md)
  – Recent and upcoming game scores.
- [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md)
  – Utility to get the current PWHL season year.
- [`load_pwhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_pbp.md)
  – Load pre-scraped PWHL play-by-play data.
- [`load_pwhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_player_box.md)
  – Load pre-scraped PWHL player box scores.
- [`load_pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_schedule.md)
  – Load pre-scraped PWHL schedules.
- [`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md)
  – Load pre-scraped PWHL team rosters.
- [`update_pwhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_pwhl_db.md)
  – Update or create a PWHL play-by-play database.

#### **New NHL Functions**

- [`nhl_where_to_watch()`](https://fastRhockey.sportsdataverse.org/reference/nhl_where_to_watch.md)
  – streaming/broadcast availability.
- [`nhl_partner_game_odds()`](https://fastRhockey.sportsdataverse.org/reference/nhl_partner_game_odds.md)
  – partner game odds by country.
- NHL API migration to `api-web.nhle.com`:
  [`nhl_game_feed()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_feed.md),
  [`nhl_game_boxscore()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_boxscore.md),
  [`nhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_schedule.md),
  [`nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams.md),
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md),
  [`nhl_player_info()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_info.md),
  [`nhl_player_game_log()`](https://fastRhockey.sportsdataverse.org/reference/nhl_player_game_log.md),
  [`nhl_stats_goalies()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_goalies.md),
  [`nhl_stats_skaters()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_skaters.md),
  [`nhl_stats_teams()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_teams.md),
  [`nhl_stats_misc()`](https://fastRhockey.sportsdataverse.org/reference/nhl_stats_misc.md),
  [`nhl_gamecenter_landing()`](https://fastRhockey.sportsdataverse.org/reference/nhl_gamecenter_landing.md),
  [`nhl_gamecenter_right_rail()`](https://fastRhockey.sportsdataverse.org/reference/nhl_gamecenter_right_rail.md),
  [`nhl_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/nhl_scoreboard.md),
  [`nhl_scores()`](https://fastRhockey.sportsdataverse.org/reference/nhl_scores.md),
  [`nhl_seasons()`](https://fastRhockey.sportsdataverse.org/reference/nhl_seasons.md),
  [`nhl_tv_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_tv_schedule.md),
  [`nhl_game_story()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_story.md),
  [`nhl_game_content()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_content.md),
  [`nhl_game_shifts()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_shifts.md),
  [`nhl_playoff_bracket()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_bracket.md),
  [`nhl_playoff_schedule()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_schedule.md),
  [`nhl_playoff_carousel()`](https://fastRhockey.sportsdataverse.org/reference/nhl_playoff_carousel.md).
- xG model integration via
  [`helper_nhl_calculate_xg()`](https://fastRhockey.sportsdataverse.org/reference/helper_nhl_calculate_xg.md).

#### **Bug Fixes**

- Fixed
  [`nhl_draft_year()`](https://fastRhockey.sportsdataverse.org/reference/nhl_draft_year.md)
  – NHL API removed the `/v1/draft/picks/{year}` endpoint. The function
  now iterates over rounds 1-7 using `/v1/draft/picks/{year}/{round}`.
- Fixed
  [`pwhl_stats()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_stats.md)
  – resolved “object ‘players’ not found” error when API calls failed.
  Fixed team ID resolution for skater stats.
- Fixed
  [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md)
  – the `season` column was inadvertently dropped from the output. It is
  now included.
- Fixed
  [`refresh_xg_models()`](https://fastRhockey.sportsdataverse.org/reference/refresh_xg_models.md)
  – resolved “cannot change value of locked binding” error by storing xG
  models in a package environment (`.xg_env`) instead of top-level
  bindings.
- Fixed NAMESPACE: removed `import(tidyverse)` which violated CRAN
  policy. Individual packages (dplyr, tidyr, etc.) are already imported.

#### **Deprecations**

- PHF functions:
  [`phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/phf_schedule.md),
  [`phf_standings()`](https://fastRhockey.sportsdataverse.org/reference/phf_standings.md),
  [`phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/phf_pbp.md),
  [`phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_box.md),
  [`phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_box.md),
  [`phf_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_roster.md),
  [`phf_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_stats.md),
  [`phf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_stats.md),
  [`phf_leaders()`](https://fastRhockey.sportsdataverse.org/reference/phf_leaders.md),
  [`phf_league_info()`](https://fastRhockey.sportsdataverse.org/reference/phf_league_info.md),
  [`phf_game_all()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_all.md),
  [`phf_game_raw()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_raw.md),
  [`phf_game_details()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_details.md),
  [`phf_game_summary()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_summary.md).
- PHF loaders:
  [`load_phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_pbp.md),
  [`load_phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_team_box.md),
  [`load_phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_player_box.md),
  [`load_phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_schedule.md),
  [`load_phf_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_rosters.md),
  [`update_phf_db()`](https://fastRhockey.sportsdataverse.org/reference/update_phf_db.md).
- Utility:
  [`most_recent_phf_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_phf_season.md).

#### **Improvements**

- [`pwhl_season_id()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_season_id.md)
  now retrieves season data dynamically from the HockeyTech API instead
  of using a hardcoded lookup table. Falls back to hardcoded data when
  the API is unavailable.
- Added internal helpers `.pwhl_api()`, `.pwhl_modulekit_url()`,
  `.pwhl_gc_url()`, and `.pwhl_resolve_season_id()` to reduce JSONP
  parsing boilerplate across PWHL functions.
- Added `lifecycle` package for formal deprecation management.
- Updated `testthat` dependency to `>= 3.0.0`.
- Complete test coverage for all 95 exported functions (482 tests).
- Added environment-controlled test toggles (`RUN_NHL_TESTS`,
  `RUN_PHF_TESTS`, `RUN_PWHL_TESTS`) via `tests/testthat/helper-skip.R`.
- CI workflow environment variables now match `helper-skip.R` names.
- Updated test expectations to match current API responses.
- [`nhl_where_to_watch()`](https://fastRhockey.sportsdataverse.org/reference/nhl_where_to_watch.md)
  returns `NULL` gracefully when the NHL `/v1/where-to-watch` endpoint
  is unavailable.
- Updated `_pkgdown.yml` with reorganized reference sections and
  deprecated function categories.

#### **Documentation & Infrastructure**

- Updated `CONTRIBUTING.md` with naming conventions, testing environment
  variables, conventional commits guide, and deprecation process.
- Updated PR and issue templates.
- Updated `CLAUDE.md` and `.github/copilot-instructions.md` to reflect
  v1.0.0 changes and new PWHL endpoints.
- Added `data-raw/pr_devel.md` development scratchpad.
- Added `cran_comments.md` for CRAN submission.
- Updated `_pkgdown.yml` with reorganized PWHL reference sections.
- Added R CMD check CI workflow and pkgdown deployment workflow.

## **fastRhockey 0.7.0**

#### **PWHL functions added**

- [`pwhl_schedule()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_schedule.md)
  function added.
- [`pwhl_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_team_roster.md)
  function added.
- [`pwhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/pwhl_teams.md)
  function added.

## **fastRhockey 0.6.0**

- Improved resiliency for several PHF functions, updates under the hood.

## **fastRhockey 0.5.0**

- Major improvements to NHL Game PBP Data parsing with shifts in-line
  via
  [`nhl_game_pbp()`](https://fastRhockey.sportsdataverse.org/reference/nhl_game_pbp.md)
  function added to match [`hockeyR`](https://hockeyr.netlify.app).

## **fastRhockey 0.4.2**

- Fixing issues with
  [`phf_league_info()`](https://fastRhockey.sportsdataverse.org/reference/phf_league_info.md)
  function for team name inconsistency.

## **fastRhockey 0.4.1**

- Minor logic addition for pbp parsing.
- More under the hood changes to adapt to tidyselect new version
  guidelines.
- [`load_phf_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_rosters.md)
  function added.
- [`load_nhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_rosters.md)
  function added.

## **fastRhockey 0.4.0**

CRAN release: 2022-10-25

- Updates logic to add Montreal Force to teams lists/parsing.
- Under the hood changes to adapt to tidyselect new version guidelines.

## **fastRhockey 0.3.1**

CRAN release: 2022-08-28

- Updates documentation per CRAN’s request.

## **fastRhockey 0.3.0**

CRAN release: 2022-03-25

- Add print method for all functions with a time stamp and description
  of the data.
- Add `phf_team_logos` dataset to package for reference.

## **fastRhockey 0.2.1**

- hotfix to `helper_phf_pbp_data()` penalty code.
- add [`try()`](https://rdrr.io/r/base/try.html) to function examples.

## **fastRhockey 0.2.0**

- [`espn_nhl_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_teams.md)
  function added.

## **fastRhockey 0.1.0**

CRAN release: 2021-12-10

- Prepped for CRAN.

## **fastRhockey 0.0.4**

#### Loader functions for PHF

- [`load_phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_pbp.md)
  function added.
- [`load_phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_team_box.md)
  function added.
- [`load_phf_player_box()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_player_box.md)
  function added.
- [`load_phf_schedule()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_schedule.md)
  function added.
- [`update_phf_db()`](https://fastRhockey.sportsdataverse.org/reference/update_phf_db.md)
  function added.

#### Player and Team Stats, Leaderboards

- [`phf_leaders()`](https://fastRhockey.sportsdataverse.org/reference/phf_leaders.md)
  function added.
- [`phf_standings()`](https://fastRhockey.sportsdataverse.org/reference/phf_standings.md)
  function added.
- [`phf_player_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_player_stats.md)
  function added.
- [`phf_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_stats.md)
  function added.
- [`phf_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_roster.md)
  function added.

## **fastRhockey 0.0.3**

#### Function naming convention normalization

- `load_phf_game()` –\>
  [`phf_game_all()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_all.md)
- [`load_phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_phf_pbp.md)
  –\>
  [`phf_pbp()`](https://fastRhockey.sportsdataverse.org/reference/phf_pbp.md)
- `load_phf_boxscore()` –\>
  [`phf_team_box()`](https://fastRhockey.sportsdataverse.org/reference/phf_team_box.md)
- `load_phf_raw_data()` –\>
  [`phf_game_raw()`](https://fastRhockey.sportsdataverse.org/reference/phf_game_raw.md)

## **fastRhockey 0.0.2**

- Added NHL functions from `powerplay` to `fastRhockey`.

## **fastRhockey 0.0.1**

- Added a `NEWS.md` file to track changes to the package.
