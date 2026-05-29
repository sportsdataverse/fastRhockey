# **PWHL Season IDs**

Retrieves PWHL season IDs from the HockeyTech API.

## Usage

``` r
pwhl_season_id(season = NULL, game_type = "regular")
```

## Arguments

- season:

  Unused; kept for backwards compatibility.

- game_type:

  Unused; kept for backwards compatibility. Defaults to "regular".

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| season_id | numeric | Season identifier used by the HockeyTech API. |
| season_name | character | Full season name (e.g., "2024-25 Regular Season"). |
| season_short | character | Short season name. |
| career | character | Whether this is a career-stats season. |
| playoff | character | Whether this is a playoff season. |
| start_date | character | Season start date. |
| end_date | character | Season end date. |
| season_yr | numeric | Year derived from the season name (concluding year). |
| game_type_label | character | Game type: "preseason", "regular", or "playoffs". |

## Examples

``` r
# \donttest{
  try(pwhl_season_id())
#>   season_id            season_name      season_short career playoff start_date
#> 1         9          2026 Playoffs     2026 Playoffs      1       1 2026-04-28
#> 2         8 2025-26 Regular Season       2025-26 Reg      1       0 2025-11-21
#> 3         7      2025-26 Preseason 2025-26 Preseason      0       0 2025-06-01
#> 4         6          2025 Playoffs     2025 Playoffs      1       1 2025-05-06
#> 5         5 2024-25 Regular Season       2024-25 Reg      1       0 2024-11-25
#> 6         4      2024-25 Preseason 2024-25 Preseason      0       0 2024-11-01
#> 7         3          2024 Playoffs     2024 Playoffs      1       1 2024-05-06
#> 8         1    2024 Regular Season          2024 Reg      1       0 2024-01-01
#> 9         2         2024 Preseason    2024 Preseason      0       0 2023-11-01
#>     end_date season_yr game_type_label
#> 1 2026-05-28      2026        playoffs
#> 2 2026-04-27      2026         regular
#> 3 2025-11-19      2026       preseason
#> 4 2025-06-03      2025        playoffs
#> 5 2025-05-05      2025         regular
#> 6 2024-11-29      2025       preseason
#> 7 2024-06-10      2024        playoffs
#> 8 2024-05-27      2024         regular
#> 9 2023-12-31      2024       preseason
# }
```
