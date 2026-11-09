# Games of the next match day of an ssgt

Wraps `GET https://www.shl.se/api/sports-v2/next-game-day` of the SHL
site API (www.shl.se). Unofficial use of a public source; seasons are
END years (2026 = 2025/26) and are mapped to the site's ssgt uuids
through `/statistics-v2/layout-info`.

## Usage

``` r
shl_next_game_day(season = NULL, game_type = "regular", return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (2026 = 2025/26), mapped to its ssgt through
  layout-info. None = the active ssgt (these match-day views follow the
  current season).

- game_type:

  Game type: regular (Grundserie), playoff (Slutspel), qualdown
  (relegation qualification) or playin (2023/24 on).

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| game_uuid | character | Platform game uuid (bdhvuc5tex); every gameday route takes it. |
| start_date_time | character | Scheduled start, local time (Europe/Stockholm). |
| state | character | Game state: pre-game or post-game (hyphenated; a live value was not observed). |
| overtime | logical | Whether the game went to overtime. |
| shootout | logical | Whether the game went to a shootout. |
| ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info originalSeasonSeriesGameTypeTeams\[\]. |
| home_team_status | character | Result for the team (WIN, LOSE) or record status. |
| home_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
| home_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
| home_team_code | character | Home team code (e.g. FHC); joins to gameday teamId and the code-keyed teams maps. |
| home_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
| home_team_names_short | character | Short club name ('Herr' = men's team). |
| home_team_names_long | character | Long club name ('Herr' = men's team). |
| home_team_names_full | character | Full club name ('Herr' = men's team). |
| home_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
| home_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
| home_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
| home_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |
| home_team_score | integer | Goals scored by the team in the game. |
| home_team_icon | character | URL of the team logo image. |
| away_team_status | character | Result for the team (WIN, LOSE) or record status. |
| away_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
| away_team_owner_instance_id | character | Club site instance id (e.g. fhc1_fhc); also names the logo at `sportality.cdn.s8y.se/team-logos/{id}.svg`. |
| away_team_code | character | Away team code (e.g. LHC); joins to gameday teamId and the code-keyed teams maps. |
| away_team_names_code | character | Display abbreviation; differs from the team code for some clubs (SKE vs SAIK, ÖRE vs OHK). |
| away_team_names_short | character | Short club name ('Herr' = men's team). |
| away_team_names_long | character | Long club name ('Herr' = men's team). |
| away_team_names_full | character | Full club name ('Herr' = men's team). |
| away_team_score | integer | Goals scored by the team in the game. |
| away_team_icon | character | URL of the team logo image. |
| venue_uuid | character | Arena uuid (platform key space). |
| venue_name | character | Name of the arena (venue). |
| series_uuid | character | Series uuid (platform key space; SHL = qQ9-bb0bzEWUk). |
| series_code | character | Series code (SHL, CHL). |
| series_display_name | character | Series label shown on the site (SHL, CHL). |
| away_team_names_code_site | character | Display abbreviation on the site; 'Herr' = men's team (e.g. 'DIF Herr'). |
| away_team_names_short_site | character | Short club name as shown on the site ('Herr' = men's team). |
| away_team_names_full_site | character | Full club name as shown on the site ('Herr' = men's team). |
| away_team_names_long_site | character | Long club name as shown on the site ('Herr' = men's team). |

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

[family_shl](https://fastRhockey.sportsdataverse.org/reference/family_shl.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(shl_next_game_day())
#> ── SHL Next Game Day Information from www.shl.se ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:58:40 UTC
#> # A tibble: 7 × 39
#>   game_uuid  start_date_time  state overtime shootout ssgt_uuid home_team_status
#>   <chr>      <chr>            <chr> <lgl>    <lgl>    <chr>     <chr>           
#> 1 c0z71d4baw 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> 2 fzan01j1h1 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> 3 huy9kqmnyw 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> 4 imds3e8pm2 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> 5 lossvjltt3 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> 6 ox7onn1v16 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> 7 oxbyiij4th 2026-11-12T18:0… pre-… FALSE    FALSE    qa98unlb… N/A             
#> # ℹ 32 more variables: home_team_uuid <chr>, home_team_owner_instance_id <chr>,
#> #   home_team_code <chr>, home_team_names_code <chr>,
#> #   home_team_names_short <chr>, home_team_names_long <chr>,
#> #   home_team_names_full <chr>, home_team_names_code_site <chr>,
#> #   home_team_names_short_site <chr>, home_team_names_full_site <chr>,
#> #   home_team_names_long_site <chr>, home_team_score <int>,
#> #   home_team_icon <chr>, away_team_status <chr>, away_team_uuid <chr>, …
# }
```
