# **NHL Records - Goalie Shutout Streaks**

Returns goalie shutout streak records from the NHL Records API
(`https://records.nhl.com/site/api/goalie-shutout-streak`).

## Usage

``` r
nhl_records_goalie_shutout_streak(
  cayenne_exp = NULL,
  limit = NULL,
  start = NULL
)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string.

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                  |           |                                            |
|------------------|-----------|--------------------------------------------|
| col_name         | types     | description                                |
| id               | integer   | Unique record identifier.                  |
| active_player    | logical   | Indicator of whether the player is active. |
| active_streak    | logical   | Indicator of whether the streak is active. |
| duration_min_sec | character | Streak duration (MM:SS).                   |
| duration_seconds | integer   | Streak duration in seconds.                |
| end_date         | character | Date the streak ended.                     |
| first_name       | character | Player first name.                         |
| franchise_id     | integer   | Unique franchise identifier.               |
| game_type_id     | integer   | Game type the streak belongs to.           |
| last_name        | character | Player last name.                          |
| player_id        | integer   | Unique player identifier.                  |
| saves            | logical   | Saves made during the streak.              |
| season_id        | integer   | Season identifier.                         |
| start_date       | character | Date the streak started.                   |
| team_abbrev      | character | Team abbreviation.                         |
| team_id          | integer   | Unique team identifier.                    |
| team_name        | character | Team name.                                 |

## Examples

``` r
# \donttest{
  try(nhl_records_goalie_shutout_streak(limit = 5))
#> ── NHL Records Goalie Shutout Streak ────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-13 02:49:47 UTC
#> # A tibble: 5 × 17
#>      id active_player active_streak duration_min_sec duration_seconds end_date  
#>   <int> <lgl>         <lgl>         <chr>                       <int> <chr>     
#> 1 13481 FALSE         FALSE         75:00                        4500 1996-02-17
#> 2 13447 FALSE         FALSE         75:00                        4500 1980-10-14
#> 3 13402 FALSE         FALSE         75:00                        4500 1942-12-17
#> 4 13438 FALSE         FALSE         75:00                        4500 1969-10-18
#> 5 13362 FALSE         FALSE         75:00                        4500 1926-12-30
#> # ℹ 11 more variables: first_name <chr>, franchise_id <int>,
#> #   game_type_id <int>, last_name <chr>, player_id <int>, saves <lgl>,
#> #   season_id <int>, start_date <chr>, team_abbrev <chr>, team_id <int>,
#> #   team_name <chr>
# }
```
