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
#> ℹ Data updated: 2026-04-08 02:42:41 UTC
#> # A tibble: 25 × 15
#>    game_id season_id date       game_date   status home_team        home_team_id
#>      <dbl>     <dbl> <chr>      <chr>       <chr>  <chr>                   <dbl>
#>  1     297         8 2026-03-25 Wed, Mar 25 4      Minnesota Frost             2
#>  2     298         8 2026-03-25 Wed, Mar 25 4      Seattle Torrent             8
#>  3     299         8 2026-03-27 Fri, Mar 27 4      Toronto Sceptres            6
#>  4     300         8 2026-03-28 Sat, Mar 28 4      Montréal Victoi…            3
#>  5     301         8 2026-03-29 Sun, Mar 29 4      Toronto Sceptres            6
#>  6     302         8 2026-03-29 Sun, Mar 29 4      Minnesota Frost             2
#>  7     303         8 2026-03-29 Sun, Mar 29 4      Seattle Torrent             8
#>  8     304         8 2026-04-01 Wed, Apr 1  4      New York Sirens             4
#>  9     305         8 2026-04-01 Wed, Apr 1  4      Montréal Victoi…            3
#> 10     306         8 2026-04-01 Wed, Apr 1  4      Ottawa Charge               5
#> # ℹ 15 more rows
#> # ℹ 8 more variables: home_team_code <chr>, home_score <chr>, away_team <chr>,
#> #   away_team_id <dbl>, away_team_code <chr>, away_score <chr>, period <chr>,
#> #   clock <chr>
# }
```
