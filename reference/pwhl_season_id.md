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
#>    season_id            season_name       season_short career playoff
#> 1         11 2026-27 Regular Season        2026-27 Reg      1       0
#> 2         10     2026-27 Pre-Season 2026-27 Pre-Season      0       0
#> 3          9          2026 Playoffs      2026 Playoffs      1       1
#> 4          8 2025-26 Regular Season        2025-26 Reg      1       0
#> 5          7      2025-26 Preseason  2025-26 Preseason      0       0
#> 6          6          2025 Playoffs      2025 Playoffs      1       1
#> 7          5 2024-25 Regular Season        2024-25 Reg      1       0
#> 8          4      2024-25 Preseason  2024-25 Preseason      0       0
#> 9          3          2024 Playoffs      2024 Playoffs      1       1
#> 10         1    2024 Regular Season           2024 Reg      1       0
#> 11         2         2024 Preseason     2024 Preseason      0       0
#>    start_date   end_date season_yr game_type_label
#> 1  2026-12-04 2027-04-19      2027         regular
#> 2  2026-10-01 2026-11-30      2027       preseason
#> 3  2026-04-28 2026-05-28      2026        playoffs
#> 4  2025-11-21 2026-04-27      2026         regular
#> 5  2025-06-01 2025-11-19      2026       preseason
#> 6  2025-05-06 2025-06-03      2025        playoffs
#> 7  2024-11-25 2025-05-05      2025         regular
#> 8  2024-11-01 2024-11-29      2025       preseason
#> 9  2024-05-06 2024-06-10      2024        playoffs
#> 10 2024-01-01 2024-05-27      2024         regular
#> 11 2023-11-01 2023-12-31      2024       preseason
# }
```
