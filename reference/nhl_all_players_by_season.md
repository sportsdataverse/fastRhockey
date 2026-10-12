# **NHL All Players by Season**

Aggregator helper that iterates every NHL team's season roster (via
[`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md))
and flattens forwards, defensemen, and goalies into a single tidy data
frame. Mirrors the `Helpers.all_players` convenience helper from the
`nhl-api-py` Python client.

## Usage

``` r
nhl_all_players_by_season(season, sleep_rate = 0)
```

## Arguments

- season:

  Integer four-digit year representing the start year of the season
  (e.g. `2024` for the 2024-25 season), matching the convention used by
  [`nhl_teams_roster()`](https://fastRhockey.sportsdataverse.org/reference/nhl_teams_roster.md).

- sleep_rate:

  Numeric seconds to
  [`Sys.sleep()`](https://rdrr.io/r/base/Sys.sleep.html) between
  per-team API calls. Defaults to `0` (no delay).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                |           |                                             |
|----------------|-----------|---------------------------------------------|
| col_name       | types     | description                                 |
| player_id      | integer   | Unique player identifier.                   |
| first_name     | character | Player first name.                          |
| last_name      | character | Player last name.                           |
| full_name      | character | Player full name.                           |
| sweater_number | integer   | Jersey number.                              |
| position_code  | character | Player position code.                       |
| shoots_catches | character | Handedness (shoots/catches).                |
| height_inches  | integer   | Player height in inches.                    |
| weight_pounds  | integer   | Player weight in pounds.                    |
| birth_date     | character | Player birth date.                          |
| birth_city     | character | Player birth city.                          |
| birth_country  | character | Player birth country.                       |
| headshot_url   | character | URL to the player headshot image.           |
| team_abbr      | character | Team abbreviation.                          |
| season         | numeric   | Season start year (e.g., 2024 for 2024-25). |

## Examples

``` r
# \donttest{
  try(nhl_all_players_by_season(season = 2024))
#> ── NHL All Players by Season ────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-10-12 12:08:02 UTC
#> # A tibble: 853 × 15
#>    player_id first_name last_name  full_name        sweater_number position_code
#>        <int> <chr>      <chr>      <chr>                     <int> <chr>        
#>  1   8478427 Sebastian  Aho        Sebastian Aho                20 F            
#>  2   8482809 Jackson    Blake      Jackson Blake                53 F            
#>  3   8477478 William    Carrier    William Carrier              28 F            
#>  4   8475791 Taylor     Hall       Taylor Hall                  71 F            
#>  5   8484929 Juha       Jaaska     Juha Jaaska                  54 F            
#>  6   8476873 Mark       Jankowski  Mark Jankowski               77 F            
#>  7   8482093 Seth       Jarvis     Seth Jarvis                  24 F            
#>  8   8479370 Tyson      Jost       Tyson Jost                   27 F            
#>  9   8480829 Jesperi    Kotkaniemi Jesperi Kotkani…             82 F            
#> 10   8476921 Jordan     Martinook  Jordan Martinook             48 F            
#> # ℹ 843 more rows
#> # ℹ 9 more variables: shoots_catches <chr>, height_inches <int>,
#> #   weight_pounds <int>, birth_date <chr>, birth_city <chr>,
#> #   birth_country <chr>, headshot_url <chr>, team_abbr <chr>, season <dbl>
# }
```
