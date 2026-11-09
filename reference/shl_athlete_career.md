# Player career totals by season

Wraps
`GET https://www.shl.se/api/statistics-v2/athlete/playerProfile_gameLog`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_athlete_career(
  athlete_uuid,
  master_site_instance_id = NULL,
  return_parsed = TRUE
)
```

## Arguments

- athlete_uuid:

  Required. Athlete uuid.

- master_site_instance_id:

  Site instance (shl1_shl). No effect in the drop-one probe.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| gp | numeric | Number of games played. |
| gpi | numeric | Games played with ice time. |
| ga | numeric | Number of goals conceded (goals against). |
| gaa | numeric | Goals against average (per 60 minutes). Numeric, but sent as a string in some rows and as a number in others. |
| svs | numeric | Number of saves made by the goalie. |
| svs_perc | numeric | Save %. Numeric, but sent as a string in some rows and as a number in others. |
| w | numeric | Goalie wins: games the goalie was credited with the win, overtime and shootout wins included. |
| t | numeric | Goalie ties: games that ended level with the goalie credited a tie; 0 in every sampled row, as overtime and shootouts now decide every game. |
| l | numeric | Goalie losses: games the goalie was charged with the loss, overtime and shootout losses included. |
| so | numeric | Number of shutouts (games without a goal against). |
| info_uuid | character | Athlete uuid (platform key space) in player and goalie modules, team uuid in team modules; joins to the athlete routes or `/sports-v2/teams/{team_uuid}`. |
| info_full_name | character | Given name and surname. |
| info_first_name | character | Given (first) name of the player. |
| info_last_name | character | Surname (family name) of the player. |
| info_birth_date | character | Date of birth (YYYY-MM-DD). |
| info_nationality | character | Nationality (ISO country code). |
| info_gender | character | Sex of the athlete or team (male / female). |
| info_age_value | integer | Age of the player in whole years. |
| info_age_format | character | Unit of value: years. |
| info_weight_value | integer | Weight of the player in kilograms. |
| info_weight_format | character | Unit of value: kg. |
| info_height_value | integer | Height of the player in centimetres. |
| info_height_format | character | Unit of value: cm. |
| info_jersey_number | character | Jersey (shirt) number of the player. |
| info_position | character | Position code (CE = centre, LW, RW, D, GK, F); N/A in the athlete game logs. |
| info_position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
| info_shoots | character | Shooting side (L / R; Left / Right). |
| info_team_uuid | character | Team uuid (platform key space, e.g. 087a-087aTQv9u); joins to `/sports-v2/teams/{team_uuid}` and schedule homeTeamInfo/awayTeamInfo.uuid. Not the team code. |
| info_team_instance_id | character | Club site instance id (e.g. fhc1_fhc). |
| info_team_name | character | Team name (e.g. Frölunda HC); empty in some rows. |
| info_team_code | character | Team code of the player's team (e.g. FHC). |
| info_season_stats | character | Current-season stat rows of the player (empty in the sample). |
| info_career_stats | character | Career stat rows of the player (empty in the sample). |
| info_statistics_provider | character | Stats provider of the data (statnet seen). |
| info_is_in_squad | logical | Whether the player is in the current squad. |
| info_player_id | integer | isa (StatNet) player id as an INTEGER. Join trap: play-by-play sends the same id as a STRING (player.playerId, pop/nep, assists) and map keys are strings; cast before joining. |
| info_ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info originalSeasonSeriesGameTypeTeams\[\]. |
| info_team_id | character | Team code (e.g. FHC) in gameday and standings payloads; not the team uuid. |

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
  try(shl_athlete_career(athlete_uuid = "qQ9-349c7WDv6", master_site_instance_id = "shl1_shl"))
#> ── SHL Athlete Career Information from www.shl.se ───────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:41:18 UTC
#> # A tibble: 37 × 38
#>       gp   gpi    ga   gaa   svs svs_perc     w     t     l    so info_uuid    
#>    <dbl> <dbl> <dbl> <dbl> <dbl>    <dbl> <dbl> <dbl> <dbl> <dbl> <chr>        
#>  1    10     0     0  0        0      0       0     0     0     0 qQ9-349c7WDv6
#>  2     8     8    16  2.15   145     90.1     7     0     1     0 qQ9-349c7WDv6
#>  3    47    47   125  2.7   1179     90.4    25     0    21     0 qQ9-349c7WDv6
#>  4     2     2     9  5.66    35     79.6     0     0     2     0 qQ9-349c7WDv6
#>  5    34    34    93  2.86   987     91.4    18     0    15     0 qQ9-349c7WDv6
#>  6     5     5     7  1.4    149     95.5     5     0     0     0 qQ9-349c7WDv6
#>  7    10     8    28  3.76   236     89.4     2     0     5     0 qQ9-349c7WDv6
#>  8    24    24    51  2.12   528     91.2    10     0    14     0 qQ9-349c7WDv6
#>  9     2     2     8  3.84    49     86.0     0     0     2     0 qQ9-349c7WDv6
#> 10    49    49    92  1.92  1333     93.5    30     0    18     0 qQ9-349c7WDv6
#> # ℹ 27 more rows
#> # ℹ 27 more variables: info_full_name <chr>, info_first_name <chr>,
#> #   info_last_name <chr>, info_birth_date <chr>, info_nationality <chr>,
#> #   info_gender <chr>, info_age_value <int>, info_age_format <chr>,
#> #   info_weight_value <int>, info_weight_format <chr>, info_height_value <int>,
#> #   info_height_format <chr>, info_jersey_number <chr>, info_position <chr>,
#> #   info_position_code <chr>, info_shoots <chr>, info_team_uuid <chr>, …
# }
```
