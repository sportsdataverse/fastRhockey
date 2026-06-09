# **NHL Stats API — Game Listing**

Returns the game listing from the NHL Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/game`). Supports server-side
filtering via a Cayenne expression.

## Usage

``` r
nhl_stats_game_listing(lang = "en", limit = 100, start = 0, cayenne_exp = NULL)
```

## Arguments

- lang:

  Character language code. Default `"en"`.

- limit:

  Integer maximum number of results. Default 100.

- start:

  Integer pagination start index. Default 0.

- cayenne_exp:

  Optional Cayenne filter expression string passed via the `cayenneExp`
  query parameter (e.g., `"season=20242025"`).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                        |           |                                  |
|------------------------|-----------|----------------------------------|
| col_name               | types     | description                      |
| id                     | integer   | Unique game identifier.          |
| eastern_start_time     | character | Game start time in Eastern time. |
| game_date              | character | Game date.                       |
| game_number            | integer   | Game number within the schedule. |
| game_schedule_state_id | integer   | Schedule state identifier.       |
| game_state_id          | integer   | Game state identifier.           |
| game_type              | integer   | Game type identifier.            |
| home_score             | integer   | Home team score.                 |
| home_team_id           | integer   | Home team identifier.            |
| period                 | integer   | Period reached in the game.      |
| season                 | integer   | Season (concluding year, YYYY).  |
| visiting_score         | integer   | Visiting team score.             |
| visiting_team_id       | integer   | Visiting team identifier.        |

## Examples

``` r
# \donttest{
  try(nhl_stats_game_listing())
#> ── NHL Stats Game Listing ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-09 20:29:40 UTC
#> # A tibble: 100 × 13
#>            id eastern_start_time  game_date  game_number game_schedule_state_id
#>         <int> <chr>               <chr>            <int>                  <int>
#>  1 1917020001 1917-12-19T20:00:00 1917-12-19           1                      1
#>  2 1917020002 1917-12-19T20:00:00 1917-12-19           2                      1
#>  3 1917020003 1917-12-22T20:00:00 1917-12-22           3                      1
#>  4 1917020004 1917-12-22T20:00:00 1917-12-22           4                      1
#>  5 1917020005 1917-12-26T20:00:00 1917-12-26           5                      1
#>  6 1917020006 1917-12-26T20:00:00 1917-12-26           6                      1
#>  7 1917020007 1917-12-29T20:00:00 1917-12-29           7                      1
#>  8 1917020008 1917-12-29T20:00:00 1917-12-29           8                      1
#>  9 1917020009 1918-01-02T20:00:00 1918-01-02           9                      1
#> 10 1917020010 1918-01-05T20:00:00 1918-01-05          10                      1
#> # ℹ 90 more rows
#> # ℹ 8 more variables: game_state_id <int>, game_type <int>, home_score <int>,
#> #   home_team_id <int>, period <int>, season <int>, visiting_score <int>,
#> #   visiting_team_id <int>
# }
```
