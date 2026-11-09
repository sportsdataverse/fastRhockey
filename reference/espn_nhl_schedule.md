# **Get ESPN NHL Team Schedule**

**Get ESPN NHL Team Schedule**

## Usage

``` r
espn_nhl_schedule(team_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"4"` for Pittsburgh
  Penguins).

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | character | ESPN event identifier. |
| date | character | Game date-time (ISO 8601). |
| name | character | Full game name. |
| short_name | character | Short game name. |
| season_year | integer | Season end year. |
| season_type | integer | Season type code. |
| time_valid | logical | Whether the game time is confirmed. |
| status_type_id | character | Status type identifier. |
| status_type_name | character | Status type name. |
| status_type_state | character | Status state (pre/in/post). |
| status_type_completed | logical | Whether the game is complete. |
| neutral_site | logical | Whether the game is at a neutral site. |
| attendance | integer | Game attendance. |
| venue_id | character | Venue identifier. |
| venue_full_name | character | Venue full name. |
| venue_city | character | Venue city. |
| venue_state | character | Venue state. |
| broadcast | character | Every network name, feed order, deduped, joined with ", " (e.g. "ESPNU, ESPN+"); a broadcast without names gives its market; "" when none. |
| home_id | character | Home team ESPN identifier. |
| home_name | character | Home team display name. |
| home_abbreviation | character | Home team abbreviation. |
| home_location | character | Home team city. |
| home_logo | character | Home team logo URL. |
| home_score | character | Home team score. |
| home_winner | logical | Whether the home team won. |
| away_id | character | Away team ESPN identifier. |
| away_name | character | Away team display name. |
| away_abbreviation | character | Away team abbreviation. |
| away_location | character | Away team city. |
| away_logo | character | Away team logo URL. |
| away_score | character | Away team score. |
| away_winner | logical | Whether the away team won. |
| team_id | character | ESPN team identifier (echoed from arg). |
| season | integer | Season end-year (echoed from arg). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_schedule(team_id = "4", season = 2025))
#> ── NHL Team Schedule data from ESPN.com ─────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:54 UTC
#> # A tibble: 82 × 34
#>    game_id   date            name  short_name season_year season_type time_valid
#>    <chr>     <chr>           <chr> <chr>            <int>       <int> <lgl>     
#>  1 401687604 2024-10-09T02:… Chic… CHI @ UTAH        2025           2 TRUE      
#>  2 401687620 2024-10-12T00:… Chic… CHI @ WPG         2025           2 TRUE      
#>  3 401687635 2024-10-13T02:… Chic… CHI @ EDM         2025           2 TRUE      
#>  4 401687654 2024-10-16T01:… Chic… CHI @ CGY         2025           2 TRUE      
#>  5 401687669 2024-10-18T00:… San … SJ @ CHI          2025           2 TRUE      
#>  6 401687684 2024-10-20T00:… Buff… BUF @ CHI         2025           2 TRUE      
#>  7 401687699 2024-10-23T00:… Vanc… VAN @ CHI         2025           2 TRUE      
#>  8 401687719 2024-10-26T00:… Nash… NSH @ CHI         2025           2 TRUE      
#>  9 401687731 2024-10-27T00:… Chic… CHI @ DAL         2025           2 TRUE      
#> 10 401687743 2024-10-29T01:… Chic… CHI @ COL         2025           2 TRUE      
#> # ℹ 72 more rows
#> # ℹ 27 more variables: status_type_id <chr>, status_type_name <chr>,
#> #   status_type_state <chr>, status_type_completed <lgl>, neutral_site <lgl>,
#> #   attendance <int>, venue_id <chr>, venue_full_name <chr>, venue_city <chr>,
#> #   venue_state <chr>, broadcast <chr>, home_id <chr>, home_name <chr>,
#> #   home_abbreviation <chr>, home_location <chr>, home_logo <chr>,
#> #   home_score <chr>, home_winner <lgl>, away_id <chr>, away_name <chr>, …
# }
```
