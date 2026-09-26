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
#> ℹ Data updated: 2026-09-26 06:40:58 UTC
#> # A tibble: 20 × 15
#>    game_id season_id date       game_date   status home_team        home_team_id
#>      <dbl>     <dbl> <chr>      <chr>       <chr>  <chr>                   <dbl>
#>  1     343         9 2026-05-08 Fri, May 8  4      Ottawa Charge               5
#>  2     345         9 2026-05-08 Fri, May 8  4      Minnesota Frost             2
#>  3     344         9 2026-05-10 Sun, May 10 4      Ottawa Charge               5
#>  4     347         9 2026-05-12 Tue, May 12 4      Montréal Victoi…            3
#>  5     350         9 2026-05-14 Thu, May 14 4      Montréal Victoi…            3
#>  6     351         9 2026-05-16 Sat, May 16 4      Montréal Victoi…            3
#>  7     348         9 2026-05-18 Mon, May 18 4      Ottawa Charge               5
#>  8     349         9 2026-05-20 Wed, May 20 4      Ottawa Charge               5
#>  9     353        10 2026-11-22 Sun, Nov 22 1      PWHL Las Vegas             12
#> 10     360        10 2026-11-23 Mon, Nov 23 1      Ottawa Charge               5
#> 11     354        10 2026-11-23 Mon, Nov 23 1      Minnesota Frost             2
#> 12     361        10 2026-11-23 Mon, Nov 23 1      PWHL Hamilton              11
#> 13     356        10 2026-11-23 Mon, Nov 23 1      Boston Fleet                1
#> 14     362        10 2026-11-24 Tue, Nov 24 1      PWHL Detroit               10
#> 15     355        10 2026-11-24 Tue, Nov 24 1      Vancouver Golde…            9
#> 16     357        10 2026-11-24 Tue, Nov 24 1      New York Sirens             4
#> 17     359        10 2026-11-24 Tue, Nov 24 1      Toronto Sceptres            6
#> 18     358        10 2026-11-25 Wed, Nov 25 1      Montréal Victoi…            3
#> 19     363        10 2026-11-29 Sun, Nov 29 1      PWHL San Jose              13
#> 20     364        10 2026-11-30 Mon, Nov 30 1      Seattle Torrent             8
#> # ℹ 8 more variables: home_team_code <chr>, home_score <chr>, away_team <chr>,
#> #   away_team_id <dbl>, away_team_code <chr>, away_score <chr>, period <chr>,
#> #   clock <chr>
# }
```
