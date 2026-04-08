# **Load fastRhockey NHL team box scores**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_team_box(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years associated with given NHL seasons. (Min:
  2011)

- ...:

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the team box data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_team_box(2021))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 2,802 × 16
#>    home_away team_id team_abbrev team_name      goals shots_on_goal   pim  hits
#>    <chr>       <int> <chr>       <chr>          <int>         <int> <int> <int>
#>  1 away            5 PIT         Penguins           6            35     2    28
#>  2 home           14 TBL         Lightning          2            28     0    31
#>  3 away           55 SEA         Kraken             3            31     6    33
#>  4 home           54 VGK         Golden Knights     4            30     8    26
#>  5 away            8 MTL         Canadiens          1            32     6    32
#>  6 home           10 TOR         Maple Leafs        2            30     8    14
#>  7 away            3 NYR         Rangers            1            24    18    27
#>  8 home           15 WSH         Capitals           5            27    16    12
#>  9 away           16 CHI         Blackhawks         2            34    10    17
#> 10 home           21 COL         Avalanche          4            36    12    25
#> # ℹ 2,792 more rows
#> # ℹ 8 more variables: blocked_shots <int>, giveaways <int>, takeaways <int>,
#> #   power_play_goals <int>, faceoff_win_pctg <dbl>, saves <int>,
#> #   save_pctg <dbl>, goals_against <int>
# }
```
