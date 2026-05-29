# **NHL Stats API — Skater Milestones**

Returns skater milestone achievements from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/milestones/skaters`).

## Usage

``` r
nhl_stats_skater_milestones(
  lang = "en",
  cayenne_exp = NULL,
  limit = 100,
  start = 0
)
```

## Arguments

- lang:

  Character language code. Default `"en"`.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter.

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer pagination start index. Default 0.

## Value

A data frame (`fastRhockey_data`) of skater milestones, or `NULL` on
failure, with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique milestone record identifier. |
| assists | integer | Assists. |
| current_team_id | integer | Player's current team identifier. |
| first_name | character | Player first name. |
| game_type_id | integer | Game type identifier (regular season / playoffs). |
| games_played | integer | Games played. |
| goals | integer | Goals scored. |
| last_name | character | Player last name. |
| milestone | character | Milestone category. |
| milestone_amount | integer | Milestone threshold amount. |
| player_full_name | character | Player full name. |
| player_id | integer | Unique player identifier. |
| points | integer | Total points (goals + assists). |
| team_abbrev | character | Team abbreviation. |
| team_common_name | character | Team common name. |
| team_full_name | character | Team full name. |
| team_place_name | character | Team place (city) name. |

## Examples

``` r
# \donttest{
  try(nhl_stats_skater_milestones())
#> ── NHL Stats Skater Milestones ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:47:46 UTC
#> # A tibble: 100 × 17
#>       id assists current_team_id first_name   game_type_id games_played goals
#>    <int>   <int>           <int> <chr>               <int>        <int> <int>
#>  1  1227     199              16 Ryan                    2          566    76
#>  2  1360      19               2 Jean-Gabriel            3           86    24
#>  3  1576      47               5 Erik                    3           73     9
#>  4  1776      33              18 Roman                   3           91    12
#>  5  2393      30               2 Brayden                 3           82    13
#>  6  2402      45              14 Brayden                 3           99    45
#>  7  2423      27              16 Andre                   3           93    20
#>  8  2425      27              16 Andre                   3           93    20
#>  9  2926      67               5 Kris                    3          155    25
#> 10  3077      70              15 Alex                    3          161    77
#> # ℹ 90 more rows
#> # ℹ 10 more variables: last_name <chr>, milestone <chr>,
#> #   milestone_amount <int>, player_full_name <chr>, player_id <int>,
#> #   points <int>, team_abbrev <chr>, team_common_name <chr>,
#> #   team_full_name <chr>, team_place_name <chr>
# }
```
