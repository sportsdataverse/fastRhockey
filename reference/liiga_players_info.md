# **Liiga roster**

Roster and bio of every player in a season range.

## Usage

``` r
liiga_players_info(
  season_from = NULL,
  season_to = NULL,
  tournament = "runkosarja"
)
```

## Arguments

- season_from:

  First season (END year) of the range.

- season_to:

  Last season (END year) of the range.

- tournament:

  Tournament: runkosarja (regular season), playoffs, playout,
  qualifications, valmistavat_ottelut (preseason) or chl (Champions
  Hockey League). Required by the API; defaults to runkosarja.

## Value

A `fastRhockey_data` tibble (23 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `player_id` | integer | Player id; the same number in lineups, rosters, stat tables and the profile (fihaId). |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `team_name` | character | Team's display name, e.g. HPK or Tappara. |
| `country_of_birth` | character | Country of birth, 3-letter code. |
| `place_of_birth` | character | Player's place of birth (town or city). |
| `date_of_birth` | character | Date of birth, YYYY-MM-DD. |
| `nationality` | character | Nationality, 3-letter code. |
| `first_name` | character | Player's first name. |
| `last_name` | character | Player's last name. |
| `role` | character | Position: CENTER, LEFT_WING, RIGHT_WING, LEFT_DEFENSEMAN, RIGHT_DEFENSEMAN, SEVENTH_DEFENSEMAN, GOALIE. |
| `role_code` | character | Position code, Finnish: KH = centre, VL/OL = left/right wing, VP/OP = left/right defence, 7. P = seventh defenceman, MV = goalkeeper. |
| `handedness` | character | Shoots LEFT or RIGHT. |
| `height` | integer | Player's height in centimetres. |
| `weight` | integer | Player's weight in kilograms. |
| `rookie` | logical | True when Liiga flags the player as a rookie. |
| `jersey` | integer | Jersey (sweater) number the player wore. |
| `injured` | logical | True when the player is listed as injured. |
| `suspended` | logical | True when the player is listed as suspended. |
| `removed` | logical | Removed from the roster. |
| `captain` | logical | True when the player is the team captain (C). |
| `alternate_captain` | logical | True when the player is an alternate captain (A). |
| `picture_url` | character | URL of the player's photo. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/players/info`

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
  try(liiga_players_info(season_from = 2026, season_to = 2026))
#> ── Liiga roster Information from liiga.fi ───────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 22:54:14 UTC
#> # A tibble: 633 × 23
#>    player_id    team_id team_slug team_name country_of_birth place_of_birth
#>        <int>      <int> <chr>     <chr>     <chr>            <chr>         
#>  1  21415137  219244634 jyp       JYP       FIN              Vantaa        
#>  2  21468906  362185137 tappara   Tappara   FIN              Lempäälä      
#>  3  21494006  495643563 kärpät    Kärpät    FIN              Kirkkonummi   
#>  4  21499603   55786244 hpk       HPK       FIN              Turku         
#>  5  21589720  362185137 tappara   Tappara   FIN              Porvoo        
#>  6  21785090  168761288 hifk      HIFK      FIN              Eurajoki      
#>  7  21808485  219244634 jyp       JYP       FIN              Jyväskylän mlk
#>  8  21819267  495643563 kärpät    Kärpät    FIN              Oulu          
#>  9  21848539  679171680 ässät     Ässät     FIN              Pirkkala      
#> 10  21851458 1368624751 k-espoo   K-Espoo   GBR              Lontoo        
#> # ℹ 623 more rows
#> # ℹ 17 more variables: date_of_birth <chr>, nationality <chr>,
#> #   first_name <chr>, last_name <chr>, role <chr>, role_code <chr>,
#> #   handedness <chr>, height <int>, weight <int>, rookie <lgl>, jersey <int>,
#> #   injured <lgl>, suspended <lgl>, removed <lgl>, captain <lgl>,
#> #   alternate_captain <lgl>, picture_url <chr>
# }
```
