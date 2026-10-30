# **HockeyAllsvenskan Player directory**

One team's players from the CMS directory: CMS ids, position, jersey,
birth date, size, headshots. One team per call (league-wide paging is
unreliable).

Ids are CMS ids (`player_cms_id`, `player_document_id`, `team_cms_id`),
not StatNet ids; `team_code` is the team's display code and `slug` joins
[`ha_leaders()`](https://fastRhockey.sportsdataverse.org/reference/ha_leaders.md).
One team per call: the directory pages unreliably league-wide, so only
one complete page of that team's players is accepted.

## Usage

``` r
ha_players(
  team,
  order_by = "familyName",
  search_query = NULL,
  return_parsed = TRUE
)
```

## Arguments

- team:

  Team display code (CMS short name: AIK, MORA, ÖIK, ...). Required: the
  directory pages unreliably league-wide.

- order_by:

  Sort: familyName or firstName. Allowed: `familyName`, `firstName`.

- search_query:

  Free-text name search; omitted = everyone. `NULL` = not sent.

- return_parsed:

  Parse the payload into a tibble (default `TRUE`). Pass `FALSE` for the
  raw JSON as a list (stamped with the request under `_request` where
  the body does not say what it answers).

## Value

A tibble (`return_parsed = TRUE`), else the raw JSON list:

|  |  |  |
|----|----|----|
| col_name | type | description |
| family_name | character | Family name of the player. |
| first_name | character | First name of the player. |
| player_cms_id | integer | CMS (Strapi) record id of the player (not the StatNet id). |
| player_document_id | character | CMS (Strapi) document id of the player. |
| headshots_large | character | Headshot URL (large rendition). |
| headshots_small | character | Headshot URL (small rendition). |
| headshots_thumb | character | Headshot URL (thumb rendition). |
| headshots_medium | character | Headshot URL (medium rendition). |
| position_code | character | Playing position code (CE centre, LW left wing, ...); positionName spells it out in English. |
| jersey_number | integer | Jersey number of the player. |
| birth_date | character | Birth date, YYYY-MM-DD. |
| country | character | Nationality, IOC/ISO alpha-3 code (SWE, NOR). |
| height | integer | Player height in centimetres. |
| weight | integer | Player weight in kilograms. |
| shoots | character | Shoots / catches: L or R. |
| youth_team | character | Youth (development) club. |
| is_rookie | logical | True when the CMS directory flags the player as a rookie. |
| team_player | character | Card label 'TEAM // POS JERSEY // Family name'. |
| slug | character | Player slug; joins ha_leaders. |
| position_name | character | Playing position, English label (Center, Left Wing, ...). |
| team_cms_id | integer | CMS (Strapi) record id of the team (81 = AIK). |
| team_document_id | character | CMS (Strapi) document id of the team. |
| team_code | character | Display code of the team (AIK, MoDo, MORA, NVIF, ÖIK), not the StatNet id. |
| team_main_color_hex | character | Primary colour of the team (hex). |
| team_name | character | Team label as shown on the card (the display team code). |
| team_logo_url | character | Logo URL of the team. |

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
[`ha_schedule()`](https://fastRhockey.sportsdataverse.org/reference/ha_schedule.md),
[`ha_skater_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_skater_stats.md),
[`ha_standings()`](https://fastRhockey.sportsdataverse.org/reference/ha_standings.md),
[`ha_team_stats()`](https://fastRhockey.sportsdataverse.org/reference/ha_team_stats.md),
[`ha_teams()`](https://fastRhockey.sportsdataverse.org/reference/ha_teams.md)

## Examples

``` r
# \donttest{
  try(ha_players(team = "AIK"))
#> ── HA Players Information from hockeyallsvenskan.se ── fastRhockey 1.0.0.9000 ──
#> ℹ Data updated: 2026-10-30 00:12:15 UTC
#> # A tibble: 30 × 26
#>    family_name first_name  player_cms_id player_document_id      headshots_large
#>    <chr>       <chr>               <int> <chr>                   <chr>          
#>  1 Bengtsson   Christopher          2690 xk3ojogy15d4kcx4htv30z… https://ha-med…
#>  2 Björk       Christoffer          2695 b4b8exr9vlq9vw3m7s32jb… https://ha-med…
#>  3 Boije       Sid                  2368 vom6f8tbmwfffw44no38p8… NA             
#>  4 Brattström  Victor               2683 dawjoxbwz5xzrjaa5578ob… https://ha-med…
#>  5 Contreras   Leon                 2082 i6q1jra32l6tvt0givzw8a… NA             
#>  6 Enroth      Jhonas               2685 l8adior0jcfjisdz9r15aa… https://ha-med…
#>  7 Eriksson    Max                  1810 nei92n4kb77qrjtzv1guwr… NA             
#>  8 Grönlund    Anders               2700 hqg7rjrije0l5ku3dcrh1q… https://ha-med…
#>  9 Hellgren    Ludwig               2750 x4wyyhq9avlg3a2ege8tro… NA             
#> 10 Isén        Edvin                2701 u866bnxgqrns6wk5zkiplf… https://ha-med…
#> # ℹ 20 more rows
#> # ℹ 21 more variables: headshots_small <chr>, headshots_thumb <chr>,
#> #   headshots_medium <chr>, position_code <chr>, jersey_number <int>,
#> #   birth_date <chr>, country <chr>, height <int>, weight <int>, shoots <chr>,
#> #   youth_team <chr>, is_rookie <lgl>, team_player <chr>, slug <chr>,
#> #   position_name <chr>, team_cms_id <int>, team_document_id <chr>,
#> #   team_code <chr>, team_main_color_hex <chr>, team_name <chr>, …
# }
```
