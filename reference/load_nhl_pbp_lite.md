# **Load fastRhockey NHL play-by-play (lite)**

Same as
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
but without line change (CHANGE) events, resulting in smaller file
sizes.

## Usage

``` r
load_nhl_pbp_lite(
  seasons = most_recent_nhl_season(),
  ...,
  dbConnection = NULL,
  tablename = NULL
)
```

## Arguments

- seasons:

  A vector of 4-digit years (the *end year* of the NHL season; e.g.,
  2026 for the 2025-26 season). Min: 2011.

- ...:

  Additional arguments passed to an underlying function.

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the data table within the database

## Value

A data frame (`fastRhockey_data`) with the same columns as
[`load_nhl_pbp()`](https://fastRhockey.sportsdataverse.org/reference/load_nhl_pbp.md)
but excluding line-change (`CHANGE`) events:

|  |  |  |
|----|----|----|
| col_name | types | description |
| event_type | character | Standardized event type code. |
| event | character | Event description label. |
| secondary_type | character | Secondary event type (e.g. shot type). |
| event_team_abbr | character | Abbreviation of the team credited with the event. |
| event_team_type | character | Whether the event team is home or away. |
| description | character | Full text description of the event. |
| period | integer | Period number. |
| period_type | character | Period type (REGULAR/OVERTIME/SHOOTOUT). |
| period_time | character | Elapsed time in the period (MM:SS). |
| period_seconds | integer | Elapsed seconds in the period. |
| game_seconds | integer | Elapsed seconds in the game. |
| home_score | integer | Home team score after the event. |
| away_score | integer | Away team score after the event. |
| event_player_1_name | character | Name of the primary event player. |
| event_player_1_id | integer | Player id of the primary event player. |
| event_player_2_name | character | Name of the secondary event player. |
| event_player_2_id | integer | Player id of the secondary event player. |
| event_goalie_name | character | Name of the goalie on the event. |
| event_goalie_id | integer | Player id of the goalie on the event. |
| strength_state | character | Strength state (e.g. 5v5, 5v4). |
| x | integer | Raw x-coordinate of the event. |
| y | integer | Raw y-coordinate of the event. |
| shot_distance | numeric | Distance of the shot from the net. |
| shot_angle | numeric | Angle of the shot relative to the net. |
| game_id | integer | Unique game identifier. |
| season | integer | Season (concluding year, YYYY). |
| xg | numeric | Expected goals value for the shot event. |

## Examples

``` r
# \donttest{
  try(load_nhl_pbp_lite(2026))
#> ─────────────────────────────────────────────────────────── fastRhockey 1.0.0 ──
#> # A tibble: 442,031 × 94
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <chr>      
#>  1 FACEOFF      Face… NA             CHI             away            Jason Dick…
#>  2 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  3 BLOCKED_SHOT Bloc… NA             CHI             away            Brad March…
#>  4 STOP         Stop… NA             NA              NA              Stoppage i…
#>  5 FACEOFF      Face… NA             CHI             away            Jason Dick…
#>  6 GIVEAWAY     Give… NA             FLA             home            Giveaway b…
#>  7 GIVEAWAY     Give… NA             CHI             away            Giveaway b…
#>  8 BLOCKED_SHOT Bloc… NA             CHI             away            Mackie Sam…
#>  9 STOP         Stop… NA             NA              NA              Stoppage i…
#> 10 FACEOFF      Face… NA             FLA             home            Jesper Boq…
#> # ℹ 442,021 more rows
#> # ℹ 88 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <int>, period_seconds_remaining <int>,
#> #   period_time_remaining <chr>, game_seconds <int>,
#> #   game_seconds_remaining <int>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
# }
```
