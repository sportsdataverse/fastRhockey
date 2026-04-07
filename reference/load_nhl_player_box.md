# **Load fastRhockey NHL player box scores**

helper that loads multiple seasons from the data repo either into memory
or writes it into a db using some forwarded arguments in the dots

## Usage

``` r
load_nhl_player_box(
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

  The name of the player box data table within the database

## Value

Returns a tibble

## Examples

``` r
# \donttest{
  try(load_nhl_player_box(2021))
#> ── NHL Player Boxscore Information from fastRhockey data repository ────────────
#> ℹ Data updated: 2023-01-05 10:02:14 UTC
#> # A tibble: 36,276 × 50
#>    player_id player_full_name  link   shoots_catches roster_status jersey_number
#>        <int> <chr>             <chr>  <chr>          <chr>         <chr>        
#>  1   8476967 Brett Kulak       /api/… L              Y             77           
#>  2   8476469 Joel Armia        /api/… R              Y             40           
#>  3   8471679 Carey Price       /api/… L              Y             31           
#>  4   8470642 Shea Weber        /api/… R              Y             6            
#>  5   8470621 Corey Perry       /api/… R              Y             94           
#>  6   8478133 Jake Evans        /api/… R              Y             71           
#>  7   8481014 Alexander Romanov /api/… L              Y             27           
#>  8   8473507 Jeff Petry        /api/… R              Y             26           
#>  9   8474038 Paul Byron        /api/… L              Y             41           
#> 10   8475726 Tyler Toffoli     /api/… R              Y             73           
#> # ℹ 36,266 more rows
#> # ℹ 44 more variables: position_code <chr>, position_name <chr>,
#> #   position_type <chr>, position_abbreviation <chr>,
#> #   skater_stats_time_on_ice <chr>, skater_stats_assists <int>,
#> #   skater_stats_goals <int>, skater_stats_shots <int>,
#> #   skater_stats_hits <int>, skater_stats_power_play_goals <int>,
#> #   skater_stats_power_play_assists <int>, …
# }
```
