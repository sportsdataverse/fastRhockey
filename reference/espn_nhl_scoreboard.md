# **Get ESPN NHL Scoreboard**

**Get ESPN NHL Scoreboard**

## Usage

``` r
espn_nhl_scoreboard(dates = NULL, ...)
```

## Arguments

- dates:

  Optional date or date-range string in `"YYYYMMDD"` or
  `"YYYYMMDD-YYYYMMDD"` format. When `NULL` (default) the current day is
  used.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game:

|  |  |  |
|----|----|----|
| col_name | types | description |
| game_id | character | ESPN event identifier. |
| uid | character | ESPN event uid. |
| date | character | Game date-time (ISO 8601). |
| name | character | Full game name. |
| short_name | character | Short game name. |
| season_year | integer | Season end year. |
| season_type | integer | Season type code (1=pre, 2=regular, 3=post). |
| season_slug | character | Season type slug. |
| status_type_id | character | Status type identifier. |
| status_type_name | character | Status type name. |
| status_type_state | character | Status state (pre/in/post). |
| status_type_completed | logical | Whether the game is complete. |
| status_type_description | character | Status description. |
| status_type_detail | character | Status detail text. |
| status_type_short_detail | character | Short status detail. |
| status_clock | numeric | Game clock in seconds. |
| status_display_clock | character | Display clock string. |
| status_period | integer | Current period. |
| neutral_site | logical | Whether the game is at a neutral site. |
| conference_competition | character | Whether it is a conference competition. |
| attendance | integer | Game attendance. |
| venue_id | character | ESPN venue identifier. |
| venue_full_name | character | Venue full name. |
| venue_city | character | Venue city. |
| venue_state | character | Venue state. |
| venue_indoor | logical | Whether the venue is indoors. |
| broadcast | character | Every network name, feed order, deduped, joined with ", " (e.g. "ESPNU, ESPN+"); a broadcast without names gives its market; "" when none. |
| note | character | Game note or headline. |
| home_id | character | Home team ESPN identifier. |
| home_name | character | Home team name. |
| home_abbreviation | character | Home team abbreviation. |
| home_display_name | character | Home team display name. |
| home_location | character | Home team city/location. |
| home_color | character | Home team primary color hex. |
| home_alternate_color | character | Home team alternate color hex. |
| home_logo | character | Home team logo URL. |
| home_score | character | Home team score. |
| home_winner | logical | Whether the home team won. |
| home_rank | integer | Home team rank (if ranked). |
| away_id | character | Away team ESPN identifier. |
| away_name | character | Away team name. |
| away_abbreviation | character | Away team abbreviation. |
| away_display_name | character | Away team display name. |
| away_location | character | Away team city/location. |
| away_color | character | Away team primary color hex. |
| away_alternate_color | character | Away team alternate color hex. |
| away_logo | character | Away team logo URL. |
| away_score | character | Away team score. |
| away_winner | logical | Whether the away team won. |
| away_rank | integer | Away team rank (if ranked). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_scoreboard(dates = "20250110"))
#> ── NHL Scoreboard data from ESPN.com ────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:19:28 UTC
#> # A tibble: 5 × 50
#>   game_id   uid       date  name  short_name season_year season_type season_slug
#>   <chr>     <chr>     <chr> <chr> <chr>            <int>       <int> <chr>      
#> 1 401688263 s:70~l:9… 2025… Chic… CHI @ DET         2025           2 regular-se…
#> 2 401688264 s:70~l:9… 2025… Mont… MTL @ WSH         2025           2 regular-se…
#> 3 401688265 s:70~l:9… 2025… Vanc… VAN @ CAR         2025           2 regular-se…
#> 4 401688266 s:70~l:9… 2025… Los … LA @ WPG          2025           2 regular-se…
#> 5 401688267 s:70~l:9… 2025… San … SJ @ UTAH         2025           2 regular-se…
#> # ℹ 42 more variables: status_type_id <chr>, status_type_name <chr>,
#> #   status_type_state <chr>, status_type_completed <lgl>,
#> #   status_type_description <chr>, status_type_detail <chr>,
#> #   status_type_short_detail <chr>, status_clock <dbl>,
#> #   status_display_clock <chr>, status_period <int>, neutral_site <lgl>,
#> #   conference_competition <chr>, attendance <int>, venue_id <chr>,
#> #   venue_full_name <chr>, venue_city <chr>, venue_state <chr>, …
# }
```
