# **HockeyAllsvenskan Team key crosswalk**

Teams of a season with both team keys: StatNet team id \<-\> display
code (from the stats tables' teams list).

The StatNet team id \<-\> display code crosswalk of a season, read from
the goalkeeper table's `teams` list (e.g. MoDo = MODO, MORA = MIK, NVIF
= NYB, OIK = OSIK). Join standings and team stats on `team_code`,
play-by-play on `team_statnet_id`.

## Usage

``` r
ha_teams(season = NULL, phase = "HA", return_parsed = TRUE)
```

## Arguments

- season:

  Season END year (2026 = 2025/26); the site's START-year key is sent as
  `season - 1`. `NULL` = the season in progress. Stats and standings
  start in 2005/06 (2006).

- phase:

  Game type: HA regular season, HAPlayoff playoffs, HAkval playout
  (relegation series). Allowed: `HA`, `HAPlayoff`, `HAkval`.

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
| team_statnet_id | character | StatNet team id (AIK, MODO, MIK, NYB, OSIK), not the display code. |
| team_code | character | Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| name | character | Short club name (the teams list's name). |
| full_name | character | Full club name (the teams list's fullName). |
| logos_icon | character | Logo: icon-size logo URL. |
| logos_large | character | Logo: large logo URL. |
| logos_small | character | Logo: small logo rendition (500 px). |
| logos_medium | character | Logo: medium logo URL. |
| logos_xlarge | character | Logo: extra-large logo URL. |

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
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md)

## Examples

``` r
# \donttest{
  try(ha_teams(season = 2026))
#> ── HA Teams Information from hockeyallsvenskan.se ──── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-18 06:12:47 UTC
#> # A tibble: 14 × 10
#>    season team_statnet_id team_code name        full_name logos_icon logos_large
#>     <int> <chr>           <chr>     <chr>       <chr>     <chr>      <chr>      
#>  1   2026 AIK             AIK       AIK         AIK       https://h… https://ha…
#>  2   2026 AIS             AIS       Almtuna     Almtuna … https://h… https://ha…
#>  3   2026 BIK             BIK       Karlskoga   BIK Karl… https://h… https://ha…
#>  4   2026 IFB             IFB       Björklöven  IF Björk… https://h… https://ha…
#>  5   2026 IKO             IKO       Oskarshamn  IK Oskar… https://h… https://ha…
#>  6   2026 KHC             KHC       Kalmar      Kalmar HC https://h… https://ha…
#>  7   2026 MIK             MORA      Mora        Mora IK   https://h… https://ha…
#>  8   2026 MODO            MoDo      MoDo        MoDo Hoc… https://h… https://ha…
#>  9   2026 NYB             NVIF      Nybro       Nybro Vi… https://h… https://ha…
#> 10   2026 OSIK            ÖIK       Östersund   Östersun… https://h… https://ha…
#> 11   2026 SSK             SSK       Södertälje  Södertäl… https://h… https://ha…
#> 12   2026 TRO             TRO       Troja-Ljun… IF Troja… https://h… https://ha…
#> 13   2026 VHC             VHC       Vimmerby    Vimmerby… https://h… https://ha…
#> 14   2026 VIK             VIK       Västerås    Västerås… https://h… https://ha…
#> # ℹ 3 more variables: logos_small <chr>, logos_medium <chr>, logos_xlarge <chr>
# }
```
