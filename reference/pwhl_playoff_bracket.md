# **PWHL Playoff Bracket**

Retrieves the playoff bracket for a PWHL season.

## Usage

``` r
pwhl_playoff_bracket(season = most_recent_pwhl_season())
```

## Arguments

- season:

  Season (YYYY) to pull the playoff bracket from. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

## Value

A data frame with playoff bracket / series data, or NULL if unavailable.

- `series_id` - Series identifier.

- `round` - Playoff round number.

- `series_name` - Series name or label.

- `team_1_id` - First team ID.

- `team_1_name` - First team name.

- `team_1_wins` - First team series wins.

- `team_2_id` - Second team ID.

- `team_2_name` - Second team name.

- `team_2_wins` - Second team series wins.

- `series_status` - Series completion status.

- `winner_id` - Winning team ID (if series is complete).

## Examples

``` r
# \donttest{
  try(pwhl_playoff_bracket(season = 2024))
#> 2026-04-08 03:19:52.929505: Error retrieving PWHL playoff bracket. $ operator is invalid for atomic vectors
#> NULL
# }
```
