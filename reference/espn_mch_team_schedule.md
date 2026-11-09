# **Get ESPN MCH / WCH Team Schedule (site_v2) – alias for `espn_mch_schedule()`**

**Get ESPN MCH / WCH Team Schedule (site_v2) – alias for
`espn_mch_schedule()`**

## Usage

``` r
espn_mch_team_schedule(team_id, season = most_recent_nhl_season(), ...)

espn_wch_team_schedule(team_id, season = most_recent_nhl_season(), ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"104"` for
  Pittsburgh Penguins).

- season:

  Season end-year (e.g. `2025`). Defaults to
  [`most_recent_nhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_nhl_season.md).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game. Identical to
schedule.

|  |  |  |
|----|----|----|
| col_name | type | description |
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team_schedule()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_schedule.md);
columns verified against a captured MCH body.

`espn_wch_team_schedule()` is the same wrapper for women's college
hockey (league slug `womens-college-hockey`): same arguments, same
columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

schedule which this function wraps.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team_schedule(team_id = "104", season = 2025))
#> ── MENS-COLLEGE-HOCKEY Team Schedule data from ESPN.com ─── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:18 UTC
#> # A tibble: 40 × 34
#>    game_id   date            name  short_name season_year season_type time_valid
#>    <chr>     <chr>           <chr> <chr>            <int>       <int> <lgl>     
#>  1 401712014 2024-10-05T23:… Holy… HC @ BU           2025           2 TRUE      
#>  2 401712015 2024-10-12T23:… Unio… UNNY @ BU         2025           2 TRUE      
#>  3 401712016 2024-10-18T23:… UCon… CONN @ BU         2025           2 TRUE      
#>  4 401712017 2024-10-26T00:… Bost… BU @ UND          2025           2 TRUE      
#>  5 401712018 2024-10-26T23:… Bost… BU @ UND          2025           2 TRUE      
#>  6 401712019 2024-11-01T23:… Mich… MICH @ BU         2025           2 TRUE      
#>  7 401712020 2024-11-02T23:… Mich… MICH @ BU         2025           2 TRUE      
#>  8 401712021 2024-11-09T00:… UMas… UML @ BU          2025           2 TRUE      
#>  9 401712022 2024-11-09T23:… Bost… BU @ UML          2025           2 TRUE      
#> 10 401712023 2024-11-16T00:… Bost… BU @ ME           2025           2 TRUE      
#> # ℹ 30 more rows
#> # ℹ 27 more variables: status_type_id <chr>, status_type_name <chr>,
#> #   status_type_state <chr>, status_type_completed <lgl>, neutral_site <lgl>,
#> #   attendance <int>, venue_id <chr>, venue_full_name <chr>, venue_city <chr>,
#> #   venue_state <chr>, broadcast <chr>, home_id <chr>, home_name <chr>,
#> #   home_abbreviation <chr>, home_location <chr>, home_logo <chr>,
#> #   home_score <chr>, home_winner <lgl>, away_id <chr>, away_name <chr>, …
# }
# \donttest{
  try(espn_wch_team_schedule(team_id = "2137", season = 2025))
#> ── WOMENS-COLLEGE-HOCKEY Team Schedule data from ESPN.com ──────────────────────
#> ℹ Data updated: 2026-11-09 19:34:18 UTC
#> # A tibble: 2 × 34
#>   game_id   date             name  short_name season_year season_type time_valid
#>   <chr>     <chr>            <chr> <chr>            <int>       <int> <lgl>     
#> 1 401762974 2025-03-14T00:0… Bost… BU @ CLAR         2025           3 TRUE      
#> 2 401762970 2025-03-15T19:0… Clar… CLAR @ WIS        2025           3 TRUE      
#> # ℹ 27 more variables: status_type_id <chr>, status_type_name <chr>,
#> #   status_type_state <chr>, status_type_completed <lgl>, neutral_site <lgl>,
#> #   attendance <int>, venue_id <chr>, venue_full_name <chr>, venue_city <chr>,
#> #   venue_state <chr>, broadcast <chr>, home_id <chr>, home_name <chr>,
#> #   home_abbreviation <chr>, home_location <chr>, home_logo <chr>,
#> #   home_score <chr>, home_winner <lgl>, away_id <chr>, away_name <chr>,
#> #   away_abbreviation <chr>, away_location <chr>, away_logo <chr>, …
# }
```
