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
#> ℹ Data updated: 2026-07-18 17:04:10 UTC
#> # A tibble: 8 × 10
#>   player_id player_slug   position sweater_number team_id headshot team_tri_code
#>       <int> <chr>         <chr>             <int>   <int> <chr>    <chr>        
#> 1   8484144 connor-bedar… C                    98      16 https:/… CHI          
#> 2   8483548 brandon-buss… G                    32      12 https:/… CAR          
#> 3   8484801 macklin-cele… C                    71      28 https:/… SJS          
#> 4   8471675 sidney-crosb… C                    87       5 https:/… PIT          
#> 5   8484984 ivan-demidov… R                    93       8 https:/… MTL          
#> 6   8481559 jack-hughes-… C                    86       1 https:/… NJD          
#> 7   8478402 connor-mcdav… C                    97      22 https:/… EDM          
#> 8   8485366 matthew-scha… D                    48       2 https:/… NYI          
#> # ℹ 3 more variables: team_logo <chr>, sort_id <int>, name_default <chr>
# }
```
