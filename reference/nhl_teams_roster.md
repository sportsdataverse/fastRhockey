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

Returns a data frame with roster information.

## Examples

``` r
# \donttest{
  try(nhl_teams_roster(team_abbr = "TOR"))
#> ── NHL Roster ───────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:22:01 UTC
#> # A tibble: 26 × 14
#>    player_id first_name last_name full_name       sweater_number position_code
#>        <int> <chr>      <chr>     <chr>                    <int> <chr>        
#>  1   8484158 Easton     Cowan     Easton Cowan                53 F            
#>  2   8477503 Max        Domi      Max Domi                    11 F            
#>  3   8480870 Bo         Groulx    Bo Groulx                   29 F            
#>  4   8485467 Luke       Haymes    Luke Haymes                 43 F            
#>  5   8475714 Calle      Jarnkrok  Calle Jarnkrok              19 F            
#>  6   8478057 Dakota     Joshua    Dakota Joshua               81 F            
#>  7   8482720 Matthew    Knies     Matthew Knies               23 F            
#>  8   8478904 Steven     Lorentz   Steven Lorentz              18 F            
#>  9   8481711 Matias     Maccelli  Matias Maccelli             63 F            
#> 10   8479318 Auston     Matthews  Auston Matthews             34 F            
#> # ℹ 16 more rows
#> # ℹ 8 more variables: shoots_catches <chr>, height_inches <int>,
#> #   weight_pounds <int>, birth_date <chr>, birth_city <chr>,
#> #   birth_country <chr>, headshot_url <chr>, team_abbr <chr>
  try(nhl_teams_roster(team_abbr = "TOR", season = 2024))
#> ── NHL Roster ───────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-04-08 07:22:02 UTC
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
