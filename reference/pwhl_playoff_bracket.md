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
#> ── PWHL Playoff Bracket ─────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-06-12 02:23:34 UTC
#> # A tibble: 3 × 11
#>   series_letter round round_name  series_name team_1_id team_1_name  team_1_wins
#>   <chr>         <dbl> <chr>       <chr>           <dbl> <chr>              <dbl>
#> 1 A                 1 Semi-Finals Semi-Finals         6 PWHL Toronto           2
#> 2 B                 1 Semi-Finals Semi-Finals         3 PWHL Montre…           0
#> 3 C                 2 PWHL Finals PWHL Finals         1 PWHL Boston            2
#> # ℹ 4 more variables: team_2_id <dbl>, team_2_name <chr>, team_2_wins <dbl>,
#> #   winner_id <dbl>
# }
```
