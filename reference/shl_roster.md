# Current roster of a team, grouped by position

Wraps
`GET https://www.shl.se/api/sports-v2/athletes/by-team-uuid/{team_uuid}`
of the SHL site API (www.shl.se). Unofficial use of a public source;
seasons are END years (2026 = 2025/26) and are mapped to the site's ssgt
uuids through `/statistics-v2/layout-info`.

## Usage

``` r
shl_roster(team_uuid, return_parsed = TRUE)
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
| position | character | Position group label: Målvakter = goalkeepers, Backar = defencemen, Forwards. |
| position_code | character | Position code (GK, D, F; CE / LW / RW in stats rows); N/A when unknown. |
| player_type | character | Roster role of the player (unverified). |
| gender | character | Sex of the athlete or team (male / female). |
| uuid | character | Athlete uuid (platform key space; qQ9-... or 10-char form); joins to the athlete routes and stats-info stats\[\].info.uuid. Not the isa or ramses_legacy id. |
| first_name | character | Given (first) name of the player. |
| last_name | character | Surname (family name) of the player. |
| full_name | character | Given name and surname. |
| nationality | character | Nationality (ISO country code). |
| jersey_number | integer | Jersey (shirt) number of the player. |
| portrait_url | character | URL of the player's latest rendered portrait. |

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
  try(shl_roster(team_uuid = "087a-087aTQv9u"))
#> ── SHL Roster Information from www.shl.se ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:41:37 UTC
#> # A tibble: 25 × 14
#>    position  position_code player_type gender uuid          first_name last_name
#>    <chr>     <chr>         <chr>       <chr>  <chr>         <chr>      <chr>    
#>  1 Målvakter GK            athlete     male   qd1-1JVKWgydZ Tobias     Normann  
#>  2 Målvakter GK            athlete     male   qQ9-349c7WDv6 Lars       Johansson
#>  3 Backar    D             athlete     male   qQ9-be56MtJ3k Tom        Nilsson  
#>  4 Backar    D             athlete     male   qQ9-e9f8rPgRL Henrik     Tömmernes
#>  5 Backar    D             athlete     male   qRh-1LnX90eIu Linus      Högberg  
#>  6 Backar    D             athlete     male   qTK-A0F44zip  Samuel     Johannes…
#>  7 Backar    D             athlete     male   qV0-AgACxE0D3 Isac       Heens    
#>  8 Backar    D             athlete     male   qWY-4a8U2sBAd Filip      Hasa     
#>  9 Backar    D             athlete     male   qYE-6h9YI4iy9 Stefan     Milošević
#> 10 Backar    D             athlete     male   qZh-2QDdvSum1 Christian  Folin    
#> # ℹ 15 more rows
#> # ℹ 7 more variables: full_name <chr>, nationality <chr>, jersey_number <int>,
#> #   portrait_url <chr>, date_of_birth <chr>, height <int>, weight <int>
# }
```
