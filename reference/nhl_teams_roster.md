# **NHL Team Roster**

Returns the roster for a given NHL team. Uses the NHL API
(`api-web.nhle.com`).

## Usage

``` r
nhl_teams_roster(team_abbr, season = NULL)
```

## Arguments

- team_abbr:

  Character three-letter team abbreviation (e.g., "TOR").

- season:

  Integer four-digit year (e.g., 2024 for the 2024-25 season). If NULL,
  returns the current roster.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                |           |                                     |
|----------------|-----------|-------------------------------------|
| col_name       | types     | description                         |
| player_id      | integer   | Unique player identifier.           |
| first_name     | character | Player first name.                  |
| last_name      | character | Player last name.                   |
| full_name      | character | Player full name.                   |
| sweater_number | integer   | Jersey number.                      |
| position_code  | character | Player position code (F/D/G).       |
| shoots_catches | character | Handedness (shoots/catches).        |
| height_inches  | integer   | Height in inches.                   |
| weight_pounds  | integer   | Weight in pounds.                   |
| birth_date     | character | Player birth date.                  |
| birth_city     | character | Player birth city.                  |
| birth_country  | character | Player birth country.               |
| headshot_url   | character | URL to the player's headshot image. |
| team_abbr      | character | Team abbreviation.                  |

## Examples

``` r
# \donttest{
  try(nhl_teams_roster(team_abbr = "TOR"))
#> ── NHL Roster ───────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:37:36 UTC
#> # A tibble: 22 × 14
#>    player_id first_name last_name full_name        sweater_number position_code
#>        <int> <chr>      <chr>     <chr>                     <int> <chr>        
#>  1   8477503 Max        Domi      Max Domi                     11 F            
#>  2   8480870 Bo         Groulx    Bo Groulx                    29 F            
#>  3   8485467 Luke       Haymes    Luke Haymes                  43 F            
#>  4   8475714 Calle      Jarnkrok  Calle Jarnkrok               19 F            
#>  5   8478057 Dakota     Joshua    Dakota Joshua                81 F            
#>  6   8482720 Matthew    Knies     Matthew Knies                23 F            
#>  7   8478904 Steven     Lorentz   Steven Lorentz               18 F            
#>  8   8481711 Matias     Maccelli  Matias Maccelli              63 F            
#>  9   8479318 Auston     Matthews  Auston Matthews              34 F            
#> 10   8477939 William    Nylander  William Nylander             88 F            
#> # ℹ 12 more rows
#> # ℹ 8 more variables: shoots_catches <chr>, height_inches <int>,
#> #   weight_pounds <int>, birth_date <chr>, birth_city <chr>,
#> #   birth_country <chr>, headshot_url <chr>, team_abbr <chr>
  try(nhl_teams_roster(team_abbr = "TOR", season = 2024))
#> ── NHL Roster ───────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:37:36 UTC
#> # A tibble: 42 × 14
#>    player_id first_name last_name full_name       sweater_number position_code
#>        <int> <chr>      <chr>     <chr>                    <int> <chr>        
#>  1   8481720 Nick       Abruzzese Nick Abruzzese              26 F            
#>  2   8477503 Max        Domi      Max Domi                    11 F            
#>  3   8482130 Roni       Hirvonen  Roni Hirvonen               33 F            
#>  4   8480995 Pontus     Holmberg  Pontus Holmberg             29 F            
#>  5   8475714 Calle      Jarnkrok  Calle Jarnkrok              19 F            
#>  6   8481147 Reese      Johnson   Reese Johnson               71 F            
#>  7   8480144 David      Kampf     David Kampf                 64 F            
#>  8   8482720 Matthew    Knies     Matthew Knies               23 F            
#>  9   8476872 Scott      Laughton  Scott Laughton              24 F            
#> 10   8478904 Steven     Lorentz   Steven Lorentz              18 F            
#> # ℹ 32 more rows
#> # ℹ 8 more variables: shoots_catches <chr>, height_inches <int>,
#> #   weight_pounds <int>, birth_date <chr>, birth_city <chr>,
#> #   birth_country <chr>, headshot_url <chr>, team_abbr <chr>
# }
```
