# Player game log of one season

Wraps
`GET https://www.shl.se/api/statistics-v2/athlete/playerProfile_seasonGameLog`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_athlete_game_log(
  athlete_uuid,
  master_site_instance_id = NULL,
  season = NULL,
  return_parsed = TRUE
)
```

## Arguments

- athlete_uuid:

  Required. Athlete uuid.

- master_site_instance_id:

  Site instance (shl1_shl). No effect in the drop-one probe.

- season:

  Season END year (2026 = 2025/26); the runtime sends the START year
  this route takes. None = the player's default (shorter) log.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| date | character | Date of the game (YYYY-MM-DD). |
| series | character | Series id in the career log. |
| home_team | character | Home team SHORT NAME (e.g. Frölunda; may carry a trailing space), not a code. |
| home_team_icon | character | Home team logo URL. |
| guest_team | character | Away team SHORT NAME (e.g. Luleå), not a code. |
| guest_team_icon | character | Away team logo URL. |
| gp | numeric | Number of games played. |
| gpi | numeric | Games played with ice time. |
| gs | numeric | Games started (goalie log). |
| w | numeric | 1 when the goalie was credited with the win in this game (overtime and shootout included), else 0. |
| t | numeric | 1 when the goalie was credited a tie in this game, else 0; 0 in every sampled row, as overtime and shootouts now decide every game. |
| l | numeric | 1 when the goalie was charged with the loss in this game (overtime and shootout included), else 0. |
| so | numeric | Number of shutouts (games without a goal against). |
| ga | numeric | Number of goals conceded (goals against). |
| gaa | numeric | Goals against average (per 60 minutes). Numeric, but sent as a string in some rows and as a number in others. |
| soga | numeric | Shots on goal against. |
| svs | numeric | Number of saves made by the goalie. |
| svs_perc | numeric | Save %. Numeric, but sent as a string in some rows and as a number in others. |
| info_full_name | character | Given name and surname. |
| info_first_name | character | Given (first) name of the player. |
| info_last_name | character | Surname (family name) of the player. |
| info_birth_date | character | Date of birth (YYYY-MM-DD). |
| info_nationality | character | Nationality (ISO country code). |
| info_gender | character | Sex of the athlete or team (male / female). |
| info_jersey_number | character | Jersey (shirt) number of the player. |
| info_position | character | Position code (CE = centre, LW, RW, D, GK, F); N/A in the athlete game logs. |
| info_position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
| info_shoots | character | Shooting side (L / R; Left / Right). '-' placeholder in every sample. |
| info_team_name | character | Team name (e.g. Frölunda HC); empty in some rows. Empty string in every sample (placeholder). |
| info_season_stats | character | Current-season stat rows of the player (empty in the sample). |
| info_career_stats | character | Career stat rows of the player (empty in the sample). |
| info_statistics_provider | character | Stats provider of the data (statnet seen). |
| info_is_in_squad | logical | Whether the player is in the current squad. |
| info_player_id | integer | isa (StatNet) player id as an INTEGER. Join trap: play-by-play sends the same id as a STRING (player.playerId, pop/nep, assists) and map keys are strings; cast before joining. |
| info_ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info seasonSeriesGameTypeTeams\[\]. |

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
  shl_athlete_game_log(athlete_uuid = "qQ9-349c7WDv6",
    master_site_instance_id = "shl1_shl", season = 2026)
#> ── SHL Athlete Game Log Information from www.shl.se ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 12:11:19 UTC
#> # A tibble: 58 × 35
#>    date   series home_team home_team_icon guest_team guest_team_icon    gp   gpi
#>    <chr>  <chr>  <chr>     <chr>          <chr>      <chr>           <dbl> <dbl>
#>  1 2026-… "[{\"… "Luleå"   https://sport… "Frölunda… https://sporta…     1     0
#>  2 2026-… "[{\"… "Frölund… https://sport… "Luleå"    https://sporta…     1     0
#>  3 2026-… "[{\"… "Luleå"   https://sport… "Frölunda… https://sporta…     1     1
#>  4 2026-… "[{\"… "Luleå"   https://sport… "Frölunda… https://sporta…     1     0
#>  5 2026-… "[{\"… "Frölund… https://sport… "Luleå"    https://sporta…     1     0
#>  6 2026-… "[{\"… "Frölund… https://sport… "Luleå"    https://sporta…     1     0
#>  7 2026-… "[{\"… "Frölund… https://sport… "Malmö"    https://sporta…     1     1
#>  8 2026-… "[{\"… "Frölund… https://sport… "Djurgård… https://sporta…     1     0
#>  9 2026-… "[{\"… "Leksand" https://sport… "Frölunda… https://sporta…     1     1
#> 10 2026-… "[{\"… "Frölund… https://sport… "Timrå"    https://sporta…     1     0
#> # ℹ 48 more rows
#> # ℹ 27 more variables: gs <dbl>, w <dbl>, t <dbl>, l <dbl>, so <dbl>, ga <dbl>,
#> #   gaa <dbl>, soga <dbl>, svs <dbl>, svs_perc <dbl>, info_full_name <chr>,
#> #   info_first_name <chr>, info_last_name <chr>, info_birth_date <chr>,
#> #   info_nationality <chr>, info_gender <chr>, info_jersey_number <chr>,
#> #   info_position <chr>, info_position_code <chr>, info_shoots <chr>,
#> #   info_team_name <chr>, info_season_stats <chr>, info_career_stats <chr>, …
# }
```
