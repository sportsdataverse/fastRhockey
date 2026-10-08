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
#> ℹ Data updated: 2026-10-08 11:32:06 UTC
#> # A tibble: 10 × 13
#>    player_id player_slug  position sweater_number team_id headshot team_tri_code
#>        <int> <chr>        <chr>             <int>   <int> <chr>    <chr>        
#>  1   8481540 cole-caufie… R                    13       8 https:/… MTL          
#>  2   8484801 macklin-cel… C                    71      28 https:/… SJS          
#>  3   8471675 sidney-cros… C                    87       5 https:/… PIT          
#>  4   8478403 jack-eichel… C                     9      54 https:/… VGK          
#>  5   8480800 quinn-hughe… D                    43      30 https:/… MIN          
#>  6   8477492 nathan-mack… C                    29      21 https:/… COL          
#>  7   8478402 connor-mcda… C                    97      22 https:/… EDM          
#>  8   8486067 gavin-mcken… L                    92      10 https:/… TOR          
#>  9   8471214 alex-ovechk… L                     8      15 https:/… WSH          
#> 10   8485366 matthew-sch… D                    48       2 https:/… NYI          
#> # ℹ 6 more variables: team_logo <chr>, sort_id <int>, name_default <chr>,
#> #   name_cs <chr>, name_fi <chr>, name_sk <chr>
# }
```
