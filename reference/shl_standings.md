# Standings of an ssgt (gameday widget)

Wraps `GET https://www.shl.se/api/gameday/team-standings/{ssgt_uuid}` of
the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_standings(ssgt_uuid, return_parsed = TRUE)
```

## Arguments

- ssgt_uuid:

  ssgt uuid: one season + series + game type (e.g. 2025/2026 SHL regular
  season). Directory: /statistics-v2/layout-info
  originalSeasonSeriesGameTypeTeams\[\].ssgtUuid.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`); `FALSE` returns the
  raw decoded JSON as a list.

## Value

A `fastRhockey_data` tibble (the raw JSON list when
`return_parsed = FALSE`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | type | description |
| season | integer | Season END year (2026 = 2025/26). |
| ssgt_uuid | character | ssgt uuid (platform key space: one season + series + game type); label via layout-info originalSeasonSeriesGameTypeTeams\[\]. |
| team_code | character | StatNet team code (SAIK, FHC). |
| team_uuid | character | Platform team uuid. |
| team_name | character | Name of the team. |
| team_short_name | character | Short display name of the team. |
| rank | integer | Position in the table (1 = top). |
| rank_diff | integer | Change in standings rank, in places (the reference point is unverified). |
| gp | integer | Number of games played. |
| w | integer | Regulation wins. |
| t | integer | Games tied after regulation, decided in overtime or a shootout (= OTW + OTL). |
| l | integer | Regulation losses. |
| otw | integer | Overtime and shootout wins. |
| otl | integer | Overtime and shootout losses. |
| p | integer | Standings points (3 per regulation win, 2 per OT/SO win, 1 per OT/SO loss). |
| gf | integer | Number of goals scored (goals for). |
| ga | integer | Number of goals conceded (goals against). |
| diff | integer | Goal difference. |

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
  try(shl_standings(ssgt_uuid = "iuzqg7dqk9"))
#> ── SHL Standings Information from www.shl.se ────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:23:59 UTC
#> # A tibble: 14 × 18
#>    season ssgt_uuid  team_code team_uuid       team_name   team_short_name  rank
#>     <int> <chr>      <chr>     <chr>           <chr>       <chr>           <int>
#>  1   2026 iuzqg7dqk9 SAIK      50e6-50e6DYeWM  Skellefteå… "Skellefteå"        1
#>  2   2026 iuzqg7dqk9 FHC       087a-087aTQv9u  Frölunda HC "Frölunda "         2
#>  3   2026 iuzqg7dqk9 VLH       fe02-fe02mf1FN  Växjö Lake… "Växjö"             3
#>  4   2026 iuzqg7dqk9 RBK       ee93-ee93uy4oW  Rögle BK    "Rögle"             4
#>  5   2026 iuzqg7dqk9 FBK       752c-752c12zB7Z Färjestad … "Färjestad"         5
#>  6   2026 iuzqg7dqk9 BIF       1ab8-1ab8bfj7N  Brynäs IF   "Brynäs"            6
#>  7   2026 iuzqg7dqk9 LHF       1a71-1a71gTHKh  Luleå Hock… "Luleå"             7
#>  8   2026 iuzqg7dqk9 MIF       8e6f-8e6fUXJvi  Malmö Redh… "Malmö"             8
#>  9   2026 iuzqg7dqk9 DIF       2459-2459QTs1f  Djurgården… "Djurgården"        9
#> 10   2026 iuzqg7dqk9 OHK       82eb-82ebmgaJ8  Örebro Hoc… "Örebro"           10
#> 11   2026 iuzqg7dqk9 LHC       41c4-41c4BiYZU  Linköping … "Linköping"        11
#> 12   2026 iuzqg7dqk9 TIK       31d1-31d1NbSlR  Timrå IK    "Timrå"            12
#> 13   2026 iuzqg7dqk9 HV71      3db0-3db09jXTE  HV71        "HV71"             13
#> 14   2026 iuzqg7dqk9 LIF       9541-95418PpkP  Leksands IF "Leksand"          14
#> # ℹ 11 more variables: rank_diff <int>, gp <int>, w <int>, t <int>, l <int>,
#> #   otw <int>, otl <int>, p <int>, gf <int>, ga <int>, diff <int>
# }
```
