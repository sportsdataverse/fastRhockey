# **Get ESPN MCH / WCH Play-by-Play**

**Get ESPN MCH / WCH Play-by-Play**

## Usage

``` r
espn_mch_pbp(game_id, ...)

espn_wch_pbp(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble, one row per play:

|  |  |  |
|----|----|----|
| col_name | type | description |
| id | character | ESPN play identifier. |
| sequence_number | character | Play sequence number. |
| type_id | character | Play type identifier. |
| type_text | character | Play type display text. |
| text | character | Play description text. |
| away_score | integer | Away team score at time of play. |
| home_score | integer | Home team score at time of play. |
| scoring_play | logical | Whether the play resulted in a goal. |
| score_value | integer | Goal value (1 for regulation/OT goals). |
| shooting_play | logical | Whether the play was a shot attempt. |
| wallclock | character | Wall-clock timestamp (ISO 8601). |
| period_number | integer | Period number (1-3 regulation, 4+ OT/SO). |
| period_display_value | character | Period display label (e.g. "1st Period"). |
| clock_display_value | character | Game clock display string (e.g. "14:32"). |
| team_id | character | ESPN team identifier for the acting team. |
| coordinate_x | numeric | X coordinate of play location. |
| coordinate_y | numeric | Y coordinate of play location. |
| strength_id | character | Strength situation identifier. |
| strength_text | character | Strength situation text (e.g. "Even Strength"). |
| shot_info_id | character | Shot type identifier. |
| shot_info_text | character | Shot type text (e.g. "Wrist Shot"). |
| athlete_id_1 | character | Primary participant athlete identifier. |
| athlete_id_2 | character | Secondary participant athlete identifier. |
| athlete_id_3 | character | Tertiary participant athlete identifier. |
| home_team_id | character | Home team ESPN identifier. |
| home_team_name | character | Home team name. |
| home_team_location | character | Home team city/location. |
| home_team_abbrev | character | Home team abbreviation. |
| home_team_display_name | character | Home team display name. |
| home_team_color | character | Home team primary color hex. |
| home_team_alternate_color | character | Home team alternate color hex. |
| home_team_logo | character | Home team logo URL. |
| home_team_score | character | Home team final score. |
| home_team_winner | logical | Whether the home team won. |
| away_team_id | character | Away team ESPN identifier. |
| away_team_name | character | Away team name. |
| away_team_location | character | Away team city/location. |
| away_team_abbrev | character | Away team abbreviation. |
| away_team_display_name | character | Away team display name. |
| away_team_color | character | Away team primary color hex. |
| away_team_alternate_color | character | Away team alternate color hex. |
| away_team_logo | character | Away team logo URL. |
| away_team_score | character | Away team final score. |
| away_team_winner | logical | Whether the away team won. |
| game_id | character | ESPN event identifier (echoed from arg). |
| season | integer | Season end-year. |
| season_type | integer | Season type code. |
| game_date | character | Game date-time (ISO 8601). |

## Details

League slug `mens-college-hockey`. Parsed from the site-v2 `summary`
payload by the same helper as
[`espn_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_pbp.md);
columns verified against a captured mch summary body.

`espn_wch_pbp()` is the same wrapper for women's college hockey (league
slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_pbp(game_id = "401717648"))
#> ── MENS-COLLEGE-HOCKEY Play-by-Play data from ESPN.com ──── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:19 UTC
#> # A tibble: 12 × 48
#>    id             sequence_number type_id type_text text   away_score home_score
#>    <chr>          <chr>           <chr>   <chr>     <chr>       <int>      <int>
#>  1 40171764804386 1               505     Goal      Even …          0          1
#>  2 40171764804387 2               505     Goal      Even …          1          1
#>  3 40171764804388 1               509     Penalty   Penal…          0          0
#>  4 40171764804389 3               505     Goal      Even …          1          2
#>  5 40171764804390 4               505     Goal      Even …          1          3
#>  6 40171764804391 2               509     Penalty   Penal…          0          0
#>  7 40171764804392 5               505     Goal      Power…          2          3
#>  8 40171764804393 3               509     Penalty   Penal…          0          0
#>  9 40171764804394 4               509     Penalty   Penal…          0          0
#> 10 40171764804395 6               505     Goal      Even …          2          4
#> 11 40171764804396 7               505     Goal      Even …          2          5
#> 12 40171764804397 8               505     Goal      Empty…          2          6
#> # ℹ 41 more variables: scoring_play <lgl>, score_value <int>,
#> #   shooting_play <lgl>, wallclock <chr>, period_number <int>,
#> #   period_display_value <chr>, clock_display_value <chr>, team_id <chr>,
#> #   coordinate_x <dbl>, coordinate_y <dbl>, strength_id <chr>,
#> #   strength_text <chr>, shot_info_id <chr>, shot_info_text <chr>,
#> #   athlete_id_1 <chr>, athlete_id_2 <chr>, athlete_id_3 <chr>,
#> #   home_team_id <chr>, home_team_name <chr>, home_team_location <chr>, …
# }
# \donttest{
  try(espn_wch_pbp(game_id = "401762970"))
#> ── WOMENS-COLLEGE-HOCKEY Play-by-Play data from ESPN.com ── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:19 UTC
#> # A tibble: 12 × 48
#>    id             sequence_number type_id type_text text   away_score home_score
#>    <chr>          <chr>           <chr>   <chr>     <chr>       <int>      <int>
#>  1 40176297004123 1               509     Penalty   Penal…          0          0
#>  2 40176297004124 1               505     Goal      Power…          1          0
#>  3 40176297004127 2               509     Penalty   Penal…          0          0
#>  4 40176297004131 2               505     Goal      Even …          1          1
#>  5 40176297004134 3               509     Penalty   Penal…          0          0
#>  6 40176297004135 3               505     Goal      Even …          1          2
#>  7 40176297004140 4               509     Penalty   Penal…          0          0
#>  8 40176297004142 5               509     Penalty   Penal…          0          0
#>  9 40176297004143 4               505     Goal      Power…          1          3
#> 10 40176297004145 6               509     Penalty   Penal…          0          0
#> 11 40176297004146 5               505     Goal      Short…          1          4
#> 12 40176297004147 7               509     Penalty   Penal…          0          0
#> # ℹ 41 more variables: scoring_play <lgl>, score_value <int>,
#> #   shooting_play <lgl>, wallclock <chr>, period_number <int>,
#> #   period_display_value <chr>, clock_display_value <chr>, team_id <chr>,
#> #   coordinate_x <dbl>, coordinate_y <dbl>, strength_id <chr>,
#> #   strength_text <chr>, shot_info_id <chr>, shot_info_text <chr>,
#> #   athlete_id_1 <chr>, athlete_id_2 <chr>, athlete_id_3 <chr>,
#> #   home_team_id <chr>, home_team_name <chr>, home_team_location <chr>, …
# }
```
