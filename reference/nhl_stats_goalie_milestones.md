# **NHL Stats API — Goalie Milestones**

Returns goalie milestone achievements from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/milestones/goalies`).

## Usage

``` r
nhl_stats_goalie_milestones(
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

A data frame (`fastRhockey_data`) with the following columns:

|                  |           |                                          |
|------------------|-----------|------------------------------------------|
| col_name         | types     | description                              |
| id               | integer   | Unique milestone record identifier.      |
| current_team_id  | integer   | Player's current team identifier.        |
| first_name       | character | Player first name.                       |
| game_type_id     | integer   | Game type identifier.                    |
| games_played     | integer   | Games played.                            |
| last_name        | character | Player last name.                        |
| milestone        | character | Milestone category.                      |
| milestone_amount | integer   | Amount remaining to reach the milestone. |
| player_full_name | character | Player full name.                        |
| player_id        | integer   | Unique player identifier.                |
| so               | integer   | Shutouts.                                |
| team_abbrev      | character | Team abbreviation.                       |
| team_common_name | character | Team common (nickname) name.             |
| team_full_name   | character | Full team name.                          |
| team_place_name  | character | Team place (city/location) name.         |
| toi_minutes      | integer   | Time on ice in minutes.                  |
| wins             | integer   | Wins.                                    |

## Examples

``` r
# \donttest{
  try(nhl_stats_goalie_milestones())
#> ── NHL Stats Goalie Milestones ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-12 18:42:24 UTC
#> # A tibble: 44 × 17
#>       id current_team_id first_name game_type_id games_played last_name 
#>    <int>           <int> <chr>             <int>        <int> <chr>     
#>  1   535              18 Juuse                 2          467 Saros     
#>  2   549               3 Igor                  3           44 Shesterkin
#>  3   595              55 Philipp               3           47 Grubauer  
#>  4   607              26 Darcy                 2          489 Kuemper   
#>  5   627              26 Darcy                 3           40 Kuemper   
#>  6   636               1 Jake                  2          497 Allen     
#>  7   640              15 Charlie               2          170 Lindgren  
#>  8   655              21 Mackenzie             2          291 Blackwood 
#>  9   658              15 Charlie               2          170 Lindgren  
#> 10   664               2 Ilya                  2          308 Sorokin   
#> # ℹ 34 more rows
#> # ℹ 11 more variables: milestone <chr>, milestone_amount <int>,
#> #   player_full_name <chr>, player_id <int>, so <int>, team_abbrev <chr>,
#> #   team_common_name <chr>, team_full_name <chr>, team_place_name <chr>,
#> #   toi_minutes <int>, wins <int>
# }
```
