# **Liiga player**

One player's bio, teams by season and active seasons.

## Usage

``` r
liiga_player(player_id)
```

## Arguments

- player_id:

  Player id (player_id in liiga_players_info or a lineup; equals
  fihaId), e.g. 21870028.

## Value

A `fastRhockey_data` tibble (18 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `active_seasons` | character | Seasons (END year) the player appears in. |
| `team_seasons` | character | Map team id -\> seasons (END year) with that team. |
| `team_list` | character | Map team name -\> team as `<num>:<slug>`. |
| `birth_locality_country_code` | character | Player's birth country, 3-letter code (e.g. FIN). |
| `birth_locality_country_name` | character | Player's birth country name, in Finnish (e.g. Suomi). |
| `birth_locality_name` | character | Player's birth place (town or city). |
| `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `first_name` | character | Player's first name. |
| `last_name` | character | Player's last name. |
| `handedness` | character | Shoots LEFT or RIGHT. |
| `height` | integer | Player's height in centimetres. |
| `is_suspended` | logical | True when the player is listed as suspended. |
| `is_removed` | logical | Removed from the roster. |
| `nationality_code` | character | Player's nationality, 3-letter country code (e.g. FIN). |
| `nationality_name` | character | Player's nationality as a country name, in Finnish (e.g. Suomi). |
| `weight` | integer | Player's weight in kilograms. |
| `teams` | character | Map keyed by an id -\> one of the player's team seasons (season, jersey, teamId, slug). |

## Details

Endpoint: `GET https://liiga.fi/api/v2/players/info/{player_id}`

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
  try(liiga_player(player_id = 21870028))
#> ── Liiga player Information from liiga.fi ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:20:59 UTC
#> # A tibble: 1 × 18
#>   active_seasons                   team_seasons team_list birth_locality_count…¹
#>   <chr>                            <chr>        <chr>     <chr>                 
#> 1 [2027, 2026, 2025, 2024, 2023, … "{\"5578624… "{\"JYP\… FIN                   
#> # ℹ abbreviated name: ¹​birth_locality_country_code
#> # ℹ 14 more variables: birth_locality_country_name <chr>,
#> #   birth_locality_name <chr>, date_of_birth <chr>, player_id <int>,
#> #   first_name <chr>, last_name <chr>, handedness <chr>, height <int>,
#> #   is_suspended <lgl>, is_removed <lgl>, nationality_code <chr>,
#> #   nationality_name <chr>, weight <int>, teams <chr>
# }
```
