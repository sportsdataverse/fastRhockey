# **NHL Player Spotlight**

Returns the current NHL player spotlight - featured players highlighted
by the league.

## Usage

``` r
nhl_player_spotlight()
```

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                |           |                                          |
|----------------|-----------|------------------------------------------|
| col_name       | types     | description                              |
| player_id      | integer   | Unique player identifier.                |
| player_slug    | character | URL slug for the player.                 |
| position       | character | Player position.                         |
| sweater_number | integer   | Player sweater (jersey) number.          |
| team_id        | integer   | Unique team identifier.                  |
| headshot       | character | URL of the player headshot image.        |
| team_tri_code  | character | Three-letter team code.                  |
| team_logo      | character | URL of the team logo image.              |
| sort_id        | integer   | Sort order identifier for the spotlight. |
| name_default   | character | Player name (default localization).      |
| name_cs        | character | Player name (Czech localization).        |
| name_fi        | character | Player name (Finnish localization).      |
| name_sk        | character | Player name (Slovak localization).       |

## Examples

``` r
# \donttest{
  try(nhl_player_spotlight())
#> ── NHL Player Spotlight ─────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-13 20:21:36 UTC
#> # A tibble: 10 × 13
#>    player_id player_slug  position sweater_number team_id headshot team_tri_code
#>        <int> <chr>        <chr>             <int>   <int> <chr>    <chr>        
#>  1   8478427 sebastian-a… C                    20      12 https:/… CAR          
#>  2   8482445 devin-coole… G                     1      20 https:/… CGY          
#>  3   8483445 cutter-gaut… L                    61      24 https:/… ANA          
#>  4   8483457 lane-hutson… D                    48       8 https:/… MTL          
#>  5   8476453 nikita-kuch… R                    86      14 https:/… TBL          
#>  6   8477492 nathan-mack… C                    29      21 https:/… COL          
#>  7   8479318 auston-matt… C                    34      10 https:/… TOR          
#>  8   8478402 connor-mcda… C                    97      22 https:/… EDM          
#>  9   8484387 matvei-mich… R                    39       4 https:/… PHI          
#> 10   8485366 matthew-sch… D                    48       2 https:/… NYI          
#> # ℹ 6 more variables: team_logo <chr>, sort_id <int>, name_default <chr>,
#> #   name_cs <chr>, name_fi <chr>, name_sk <chr>
# }
```
