# **PWHL Season IDs**

Retrieves PWHL season IDs from the HockeyTech API.

## Usage

``` r
pwhl_season_id(season, game_type = "regular")
```

## Arguments

- season:

  Unused; kept for backwards compatibility.

- game_type:

  Unused; kept for backwards compatibility. Defaults to "regular".

## Value

A data frame with columns:

- `season_id` - Numeric season identifier used by the HockeyTech API.

- `season_name` - Full season name (e.g., "2024-25 Regular Season").

- `season_short` - Short season name.

- `season_yr` - Numeric year derived from the season name (concluding
  year).

- `game_type_label` - Game type: "preseason", "regular", or "playoffs".

- `career` - Whether this is a career-stats season.

- `playoff` - Whether this is a playoff season.

- `start_date` - Season start date.

- `end_date` - Season end date.

## Examples

``` r
# \donttest{
  try(pwhl_season_id())
#>   season_id            season_name      season_short career playoff start_date
#> 1         8 2025-26 Regular Season       2025-26 Reg      1       0 2025-11-21
#> 2         7      2025-26 Preseason 2025-26 Preseason      0       0 2025-06-01
#> 3         6          2025 Playoffs     2025 Playoffs      1       1 2025-05-06
#> 4         5 2024-25 Regular Season       2024-25 Reg      1       0 2024-11-25
#> 5         4      2024-25 Preseason 2024-25 Preseason      0       0 2024-11-01
#> 6         3          2024 Playoffs     2024 Playoffs      1       1 2024-05-06
#> 7         1    2024 Regular Season          2024 Reg      1       0 2024-01-01
#> 8         2         2024 Preseason    2024 Preseason      0       0 2023-11-01
#>     end_date season_yr game_type_label
#> 1 2026-04-27      2026         regular
#> 2 2025-11-19      2026       preseason
#> 3 2025-06-03      2025        playoffs
#> 4 2025-05-05      2025         regular
#> 5 2024-11-29      2025       preseason
#> 6 2024-06-10      2024        playoffs
#> 7 2024-05-27      2024         regular
#> 8 2023-12-31      2024       preseason
# }
```
