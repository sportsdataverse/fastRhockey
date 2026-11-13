# **Liiga team history**

Every team's regular-season record, one row per team and season since
1976.

## Usage

``` r
liiga_teams_history()
```

## Value

A `fastRhockey_data` tibble (25 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `id` | integer | Row id of this team-season row (NOT a team id; `teamId` is the team). |
| `season` | integer | Season END year (2026 = 2025-26). |
| `league_id` | integer | League id: 1 = Liiga (SM-liiga) on every /teams/info row 1976-2027. |
| `serie_id` | integer | Tournament id, the same every season: 1188 regular season, 1189 playoffs, 1194 preseason, 1890 CHL. |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `away_games` | integer | Away games (team, that regular season). |
| `away_losses` | integer | Away losses (team, that regular season). |
| `away_ties` | integer | Away ties (team, that regular season). |
| `away_wins` | integer | Away wins (team, that regular season). |
| `away_wins_in_extra_time` | integer | Away wins in extra time (team, that regular season). |
| `games` | integer | Games (team, that regular season). |
| `goals_against` | integer | Goals against (team, that regular season). |
| `goals_scored` | integer | Goals scored (team, that regular season). |
| `home_games` | integer | Home games (team, that regular season). |
| `home_losses` | integer | Home losses (team, that regular season). |
| `home_ties` | integer | Home ties (team, that regular season). |
| `home_wins` | integer | Home wins (team, that regular season). |
| `home_wins_in_extra_time` | integer | Home wins in extra time (team, that regular season). |
| `losses` | integer | Losses (team, that regular season). |
| `playoff_rank` | integer | Playoff rank (team, that regular season). |
| `points` | integer | Points (team, that regular season). |
| `regular_season_rank` | integer | Regular season rank (team, that regular season). |
| `ties` | integer | Ties (team, that regular season). |
| `wins` | integer | Wins (team, that regular season). |
| `wins_in_extra_time` | integer | Wins in extra time (team, that regular season). |

## Details

Endpoint: `GET https://liiga.fi/api/v2/teams/info` (with `team=all`)

Seasons are END years (2026 = 2025-26), the same key as
sportsdataverse-py. Requests to liiga.fi are spaced at least one second
apart. A request Liiga answers with nothing (an unknown game, a
tournament not played that season, a shot map before 2016) raises
`fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
`fastRhockey_asset_fetch`; a bad argument raises
`fastRhockey_bad_request`.

## See also

[family_liiga](https://fastRhockey.sportsdataverse.org/reference/family_liiga.md)
lists every function in this family.

## Examples

``` r
# \donttest{
  try(liiga_teams_history())
#> ── Liiga team history Information from liiga.fi ─────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:19:50 UTC
#> # A tibble: 593 × 25
#>       id season league_id serie_id    team_id away_games away_losses away_ties
#>    <int>  <int>     <int>    <int>      <int>      <int>       <int>     <int>
#>  1   848   2027         1     1188  624554857          9           0         3
#>  2   841   2027         1     1188  951626834         12           5         2
#>  3   843   2027         1     1188  292293444          9           3         1
#>  4   844   2027         1     1188  219244634         11           3         0
#>  5   854   2027         1     1188  679171680         12           5         2
#>  6   850   2027         1     1188  933686567          8           3         0
#>  7   853   2027         1     1188  651304385         10           5         3
#>  8   847   2027         1     1188  495643563         10           3         5
#>  9   855   2027         1     1188 1368624751         10           4         4
#> 10   852   2027         1     1188  362185137         10           4         2
#> # ℹ 583 more rows
#> # ℹ 17 more variables: away_wins <int>, away_wins_in_extra_time <int>,
#> #   games <int>, goals_against <int>, goals_scored <int>, home_games <int>,
#> #   home_losses <int>, home_ties <int>, home_wins <int>,
#> #   home_wins_in_extra_time <int>, losses <int>, points <int>,
#> #   playoff_rank <int>, regular_season_rank <int>, ties <int>, wins <int>,
#> #   wins_in_extra_time <int>
# }
```
