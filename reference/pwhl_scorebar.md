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

A data frame with recent/upcoming game scores, or NULL if unavailable.

- `game_id` - Game ID.

- `date` - Game date.

- `status` - Game status.

- `home_team` - Home team name.

- `home_team_id` - Home team ID.

- `home_score` - Home team score.

- `away_team` - Away team name.

- `away_team_id` - Away team ID.

- `away_score` - Away team score.

- `period` - Current period (for live or completed games).

- `clock` - Current clock time (for live games).

## Examples

``` r
# \donttest{
  try(pwhl_scorebar(days_back = 7, days_ahead = 7))
#> ── PWHL Scorebar ────────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-11 16:04:15 UTC
#> # A tibble: 13 × 15
#>    game_id season_id date       game_date   status home_team        home_team_id
#>      <dbl>     <dbl> <chr>      <chr>       <chr>  <chr>                   <dbl>
#>  1     329         8 2026-04-25 Sat, Apr 25 4      Boston Fleet                1
#>  2     328         8 2026-04-25 Sat, Apr 25 4      Ottawa Charge               5
#>  3     327         8 2026-04-25 Sat, Apr 25 4      Vancouver Golde…            9
#>  4     326         8 2026-04-25 Sat, Apr 25 4      Seattle Torrent             8
#>  5     338         9 2026-04-30 Thu, Apr 30 4      Boston Fleet                1
#>  6     340         9 2026-05-02 Sat, May 2  4      Montréal Victoi…            3
#>  7     339         9 2026-05-02 Sat, May 2  4      Boston Fleet                1
#>  8     341         9 2026-05-05 Tue, May 5  4      Montréal Victoi…            3
#>  9     342         9 2026-05-07 Thu, May 7  4      Minnesota Frost             2
#> 10     343         9 2026-05-08 Fri, May 8  4      Ottawa Charge               5
#> 11     345         9 2026-05-08 Fri, May 8  4      Minnesota Frost             2
#> 12     344         9 2026-05-10 Sun, May 10 4      Ottawa Charge               5
#> 13     347         9 2026-05-11 Mon, May 11 1      Montréal Victoi…            3
#> # ℹ 8 more variables: home_team_code <chr>, home_score <chr>, away_team <chr>,
#> #   away_team_id <dbl>, away_team_code <chr>, away_score <chr>, period <chr>,
#> #   clock <chr>
# }
```
