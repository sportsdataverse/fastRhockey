# **Load fastRhockey PWHL per-game rosters**

Helper that loads multiple seasons of PWHL per-game rosters (one row per
player per game, including starters and scratches) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

Source release tag: `pwhl_game_rosters`. File naming convention:
`game_rosters_{end_year}.rds`. Distinct from
[`load_pwhl_rosters()`](https://fastRhockey.sportsdataverse.org/reference/load_pwhl_rosters.md),
which returns season-level team rosters.

## Usage

``` r
load_pwhl_game_rosters(
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

  The name of the per-game rosters data table within the database

## Value

A data frame of class `fastRhockey_data`. Common columns include:

|                 |           |                                      |
|-----------------|-----------|--------------------------------------|
| column          | type      | description                          |
| `game_id`       | integer   | PWHL game id                         |
| `team_id`       | integer   | team id                              |
| `team`          | character | team name                            |
| `team_abbr`     | character | three-letter abbreviation            |
| `team_side`     | character | `"home"` or `"away"`                 |
| `player_type`   | character | `"skater"` or `"goalie"`             |
| `player_id`     | integer   | player id                            |
| `first_name`    | character | player first name                    |
| `last_name`     | character | player last name                     |
| `jersey_number` | integer   | jersey number                        |
| `position`      | character | position code                        |
| `birth_date`    | character | birth date                           |
| `starting`      | integer   | started the game (`0`/`1`)           |
| `status`        | character | status string (e.g. captain markers) |

## Examples

``` r
# \donttest{
  try(load_pwhl_game_rosters(2024))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 3,545 × 14
#>    game_id team_id team     team_abbr team_side player_type player_id first_name
#>      <int>   <int> <chr>    <chr>     <chr>     <chr>           <int> <chr>     
#>  1       2       6 PWHL To… TOR       home      skater             71 Jocelyne  
#>  2       2       6 PWHL To… TOR       home      skater            133 Lauriane  
#>  3       2       6 PWHL To… TOR       home      skater             68 Kali      
#>  4       2       6 PWHL To… TOR       home      skater            131 Olivia    
#>  5       2       6 PWHL To… TOR       home      skater            101 Alexa     
#>  6       2       6 PWHL To… TOR       home      skater             74 Allie     
#>  7       2       6 PWHL To… TOR       home      skater             67 Renata    
#>  8       2       6 PWHL To… TOR       home      skater            126 Samantha  
#>  9       2       6 PWHL To… TOR       home      skater             65 Jesse     
#> 10       2       6 PWHL To… TOR       home      skater             72 Rebecca   
#> # ℹ 3,535 more rows
#> # ℹ 6 more variables: last_name <chr>, jersey_number <int>, position <chr>,
#> #   birth_date <chr>, starting <chr>, status <chr>
# }
```
