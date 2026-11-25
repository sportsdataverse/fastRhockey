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
#> ℹ Data updated: 2026-11-25 00:42:21 UTC
#> # A tibble: 10 × 13
#>    player_id player_slug  position sweater_number team_id headshot team_tri_code
#>        <int> <chr>        <chr>             <int>   <int> <chr>    <chr>        
#>  1   8483445 cutter-gaut… L                    61      24 https:/… ANA          
#>  2   8483457 lane-hutson… D                    48       8 https:/… MTL          
#>  3   8478864 kirill-kapr… L                    97      30 https:/… MIN          
#>  4   8476453 nikita-kuch… R                    86      14 https:/… TBL          
#>  5   8477946 dylan-larki… C                    89       8 https:/… MTL          
#>  6   8478402 connor-mcda… C                    97      22 https:/… EDM          
#>  7   8479979 jake-oettin… G                    29      25 https:/… DAL          
#>  8   8483506 sam-rinzel-… D                     6      16 https:/… CHI          
#>  9   8485366 matthew-sch… D                    48       2 https:/… NYI          
#> 10   8480023 robert-thom… C                    18      19 https:/… STL          
#> # ℹ 6 more variables: team_logo <chr>, sort_id <int>, name_default <chr>,
#> #   name_cs <chr>, name_fi <chr>, name_sk <chr>
# }
```
