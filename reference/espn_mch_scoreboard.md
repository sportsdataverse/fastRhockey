# **Get ESPN MCH / WCH Scoreboard**

**Get ESPN MCH / WCH Scoreboard**

## Usage

``` r
espn_mch_scoreboard(
  dates = NULL,
  week = NULL,
  season_type = NULL,
  groups = NULL,
  limit = 500,
  ...
)

espn_wch_scoreboard(
  dates = NULL,
  week = NULL,
  season_type = NULL,
  groups = NULL,
  limit = 500,
  ...
)
```

## Arguments

- dates:

  Optional date or date-range string in `"YYYYMMDD"` or
  `"YYYYMMDD-YYYYMMDD"` format. When `NULL` (default) the current day is
  used.

- week:

  Optional week number (ESPN `week` query).

- season_type:

  Optional season phase (1 = pre, 2 = regular, 3 = post); sent as
  `seasontype`.

- groups:

  Optional conference/group id filter.

- limit:

  Maximum number of events (ESPN `limit`; default `500`, as
  sportsdataverse-py).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per game:

|  |  |  |
|----|----|----|
| col_name | type | description |
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

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_scoreboard()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_scoreboard.md);
columns verified against a captured MCH body.

`espn_wch_scoreboard()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_scoreboard(dates = "20250110"))
#> ── MENS-COLLEGE-HOCKEY Scoreboard data from ESPN.com ────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:02 UTC
#> # A tibble: 29 × 50
#>    game_id   uid      date  name  short_name season_year season_type season_slug
#>    <chr>     <chr>    <chr> <chr> <chr>            <int>       <int> <chr>      
#>  1 401715296 s:70~l:… 2025… Minn… MINN @ OSU        2025           2 regular-se…
#>  2 401711967 s:70~l:… 2025… Alas… UAA @ NIA         2025           2 regular-se…
#>  3 401711856 s:70~l:… 2025… Merc… MERC @ AR…        2025           2 regular-se…
#>  4 401711903 s:70~l:… 2025… RIT … RIT @ CAN         2025           2 regular-se…
#>  5 401712001 s:70~l:… 2025… Robe… RMU @ HC          2025           2 regular-se…
#>  6 401712011 s:70~l:… 2025… Corn… COR @ SHU         2025           2 regular-se…
#>  7 401712030 s:70~l:… 2025… Verm… UVM @ BU          2025           2 regular-se…
#>  8 401712089 s:70~l:… 2025… UCon… CONN @ UNH        2025           2 regular-se…
#>  9 401713283 s:70~l:… 2025… Unio… UNNY @ CL…        2025           2 regular-se…
#> 10 401713340 s:70~l:… 2025… Quin… QUIN @ PR…        2025           2 regular-se…
#> # ℹ 19 more rows
#> # ℹ 42 more variables: status_type_id <chr>, status_type_name <chr>,
#> #   status_type_state <chr>, status_type_completed <lgl>,
#> #   status_type_description <chr>, status_type_detail <chr>,
#> #   status_type_short_detail <chr>, status_clock <dbl>,
#> #   status_display_clock <chr>, status_period <int>, neutral_site <lgl>,
#> #   conference_competition <chr>, attendance <int>, venue_id <chr>, …
# }
# \donttest{
  try(espn_wch_scoreboard(dates = "20250110"))
#> data frame with 0 columns and 0 rows
# }
```
