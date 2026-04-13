# **Load fastRhockey PWHL shots-by-period**

Helper that loads multiple seasons of per-period shot/goal totals from
the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_shots_by_period`. File naming convention:
`shots_by_period_{end_year}.rds`.

## Usage

``` r
load_pwhl_shots_by_period(
  seasons = most_recent_pwhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given PWHL seasons. (Min:
  2024)

- ...:

  Additional arguments (currently unused; kept for API symmetry).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the shots-by-period data table within the database

## Value

A data frame of class `fastRhockey_data`. One row per period per game.
Common columns include:

|              |           |                                       |
|--------------|-----------|---------------------------------------|
| column       | type      | description                           |
| `game_id`    | integer   | PWHL game id                          |
| `period_id`  | integer   | period id (1-3, 4 = OT, 5 = SO, etc.) |
| `period`     | character | period long name                      |
| `home_goals` | integer   | home goals in period                  |
| `home_shots` | integer   | home shots in period                  |
| `away_goals` | integer   | away goals in period                  |
| `away_shots` | integer   | away shots in period                  |

## Examples

``` r
# \donttest{
  try(load_pwhl_shots_by_period(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 284 × 7
#>    game_id period_id period home_goals home_shots away_goals away_shots
#>      <int>     <int> <chr>       <int>      <int>      <int>      <int>
#>  1       2         1 1st             0          8          1         13
#>  2       2         2 2nd             0         16          0          4
#>  3       2         3 3rd             0          5          3         11
#>  4       3         1 1st             0         10          0          4
#>  5       3         2 2nd             1         12          1          9
#>  6       3         3 3rd             1          6          1         10
#>  7       3         4 1st OT          0          0          1          1
#>  8       4         1 1st             0         11          2          7
#>  9       4         2 2nd             1         11          1          7
#> 10       4         3 3rd             1         13          0          2
#> # ℹ 274 more rows
# }
```
