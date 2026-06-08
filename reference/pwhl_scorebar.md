# **PWHL Scorebar**

Retrieves recent and upcoming PWHL game scores.

## Usage

``` r
pwhl_scorebar(days_back = 3, days_ahead = 3)
```

## Arguments

- days_back:

  Number of days back to include. Default 3.

- days_ahead:

  Number of days ahead to include. Default 3.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                |           |                                          |
|----------------|-----------|------------------------------------------|
| col_name       | types     | description                              |
| game_id        | numeric   | Unique game identifier.                  |
| season_id      | numeric   | Season identifier.                       |
| date           | character | Game date.                               |
| game_date      | character | Game date.                               |
| status         | character | Status of the game.                      |
| home_team      | character | Home team name.                          |
| home_team_id   | numeric   | Home team identifier.                    |
| home_team_code | character | Home team abbreviation.                  |
| home_score     | character | Home team score.                         |
| away_team      | character | Away team name.                          |
| away_team_id   | numeric   | Away team identifier.                    |
| away_team_code | character | Away team abbreviation.                  |
| away_score     | character | Away team score.                         |
| period         | character | Current period for live/completed games. |
| clock          | character | Current clock time for live games.       |

## Examples

``` r
# \donttest{
  try(pwhl_scorebar(days_back = 7, days_ahead = 7))
#> ── PWHL Scorebar ────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-08 16:04:04 UTC
#> # A tibble: 8 × 15
#>   game_id season_id date  game_date status home_team home_team_id home_team_code
#>     <dbl>     <dbl> <chr> <chr>     <chr>  <chr>            <dbl> <chr>         
#> 1     343         9 2026… Fri, May… 4      Ottawa C…            5 OTT           
#> 2     345         9 2026… Fri, May… 4      Minnesot…            2 MIN           
#> 3     344         9 2026… Sun, May… 4      Ottawa C…            5 OTT           
#> 4     347         9 2026… Tue, May… 4      Montréal…            3 MTL           
#> 5     350         9 2026… Thu, May… 4      Montréal…            3 MTL           
#> 6     351         9 2026… Sat, May… 4      Montréal…            3 MTL           
#> 7     348         9 2026… Mon, May… 4      Ottawa C…            5 OTT           
#> 8     349         9 2026… Wed, May… 4      Ottawa C…            5 OTT           
#> # ℹ 7 more variables: home_score <chr>, away_team <chr>, away_team_id <dbl>,
#> #   away_team_code <chr>, away_score <chr>, period <chr>, clock <chr>
# }
```
