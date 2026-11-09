# **Get ESPN MCH / WCH Season Team (core-v2)**

**Get ESPN MCH / WCH Season Team (core-v2)**

## Usage

``` r
espn_mch_season_team(season = most_recent_nhl_season(), team_id, ...)

espn_wch_season_team(season = most_recent_nhl_season(), team_id, ...)
```

## Arguments

- season:

  Season end-year (e.g. `2026`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- team_id:

  ESPN team identifier (character or numeric). Use
  [`espn_mch_season_teams()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_season_teams.md)
  to discover valid team ids for a season.

- ...:

  Reserved for forward compatibility.

## Value

A one-row `fastRhockey_data` tibble with team-in-season metadata:

|                    |           |                                            |
|--------------------|-----------|--------------------------------------------|
| col_name           | types     | description                                |
| season             | integer   | Season year (echoed from arg).             |
| team_id            | character | ESPN team id (echoed from arg).            |
| id                 | character | ESPN team identifier.                      |
| guid               | character | Team global unique identifier.             |
| uid                | character | Team uid string.                           |
| slug               | character | Team URL slug.                             |
| location           | character | Team city/location.                        |
| name               | character | Team mascot/name.                          |
| nickname           | character | Team nickname.                             |
| abbreviation       | character | Team abbreviation.                         |
| display_name       | character | Team display name.                         |
| short_display_name | character | Team short display name.                   |
| color              | character | Team primary color hex.                    |
| alternate_color    | character | Team alternate color hex.                  |
| is_active          | logical   | Whether the team is currently active.      |
| is_all_star        | logical   | Whether this is an all-star team.          |
| team_ref           | character | `$ref` URL for this team-in-season object. |
| record_ref         | character | `$ref` URL for the team record.            |
| athletes_ref       | character | `$ref` URL for the team athletes.          |
| venue_ref          | character | `$ref` URL for the team venue.             |
| groups_ref         | character | `$ref` URL for the team groups.            |
| statistics_ref     | character | `$ref` URL for the team statistics.        |
| leaders_ref        | character | `$ref` URL for the team leaders.           |
| injuries_ref       | character | `$ref` URL for the team injuries.          |
| awards_ref         | character | `$ref` URL for the team awards.            |
| franchise_ref      | character | `$ref` URL for the team franchise.         |
| events_ref         | character | `$ref` URL for the team events.            |
| transactions_ref   | character | `$ref` URL for the team transactions.      |
| coaches_ref        | character | `$ref` URL for the team coaches.           |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_season_team()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_season_team.md)
(columns inherited); a collection may be empty.

`espn_wch_season_team()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try({
    tms <- espn_mch_season_teams(season = 2026)
    espn_mch_season_team(season = 2026, team_id = tms$team_id[1])
  })
#> ── MENS-COLLEGE-HOCKEY Season Team data from ESPN core-v2 ──────────────────────
#> ℹ Data updated: 2026-11-09 11:49:30 UTC
#> # A tibble: 1 × 29
#>   season team_id id    guid     uid   slug  location name  nickname abbreviation
#>    <int> <chr>   <chr> <chr>    <chr> <chr> <chr>    <chr> <chr>    <chr>       
#> 1   2026 1       1     5f45e55… s:70… alas… Alaska … Seaw… Alaska … UAA         
#> # ℹ 19 more variables: display_name <chr>, short_display_name <chr>,
#> #   color <chr>, alternate_color <chr>, is_active <lgl>, is_all_star <lgl>,
#> #   team_ref <chr>, record_ref <chr>, athletes_ref <chr>, venue_ref <chr>,
#> #   groups_ref <chr>, statistics_ref <chr>, leaders_ref <chr>,
#> #   injuries_ref <chr>, awards_ref <chr>, franchise_ref <chr>,
#> #   events_ref <chr>, transactions_ref <chr>, coaches_ref <chr>
# }
# \donttest{
  try({
    tms <- espn_wch_season_teams(season = 2026)
    espn_wch_season_team(season = 2026, team_id = tms$team_id[1])
  })
#> ── WOMENS-COLLEGE-HOCKEY Season Team data from ESPN core-v2 ────────────────────
#> ℹ Data updated: 2026-11-09 11:49:30 UTC
#> # A tibble: 1 × 29
#>   season team_id id    guid     uid   slug  location name  nickname abbreviation
#>    <int> <chr>   <chr> <chr>    <chr> <chr> <chr>    <chr> <chr>    <chr>       
#> 1   2026 41      41    eaa8057… s:70… ucon… UConn    Husk… UConn    CONN        
#> # ℹ 19 more variables: display_name <chr>, short_display_name <chr>,
#> #   color <chr>, alternate_color <chr>, is_active <lgl>, is_all_star <lgl>,
#> #   team_ref <chr>, record_ref <chr>, athletes_ref <chr>, venue_ref <chr>,
#> #   groups_ref <chr>, statistics_ref <chr>, leaders_ref <chr>,
#> #   injuries_ref <chr>, awards_ref <chr>, franchise_ref <chr>,
#> #   events_ref <chr>, transactions_ref <chr>, coaches_ref <chr>
# }
```
