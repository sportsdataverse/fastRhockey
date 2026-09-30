# **NHL Player Spotlight**

Returns the current NHL player spotlight — featured players highlighted
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
#> ── NHL Player Spotlight ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-09-30 14:44:51 UTC
#> # A tibble: 10 × 13
#>    player_id player_slug  position sweater_number team_id headshot team_tri_code
#>        <int> <chr>        <chr>             <int>   <int> <chr>    <chr>        
#>  1   8480803 evan-boucha… D                     2      22 https:/… EDM          
#>  2   8484801 macklin-cel… C                    71      28 https:/… SJS          
#>  3   8471675 sidney-cros… C                    87       5 https:/… PIT          
#>  4   8481559 jack-hughes… C                    86       1 https:/… NJD          
#>  5   8477492 nathan-mack… C                    29      21 https:/… COL          
#>  6   8478402 connor-mcda… C                    97      22 https:/… EDM          
#>  7   8471214 alex-ovechk… L                     8      15 https:/… WSH          
#>  8   8477956 david-pastr… R                    88       6 https:/… BOS          
#>  9   8485366 matthew-sch… D                    48       2 https:/… NYI          
#> 10   8476883 andrei-vasi… G                    88      14 https:/… TBL          
#> # ℹ 6 more variables: team_logo <chr>, sort_id <int>, name_default <chr>,
#> #   name_cs <chr>, name_fi <chr>, name_sk <chr>
# }
```
