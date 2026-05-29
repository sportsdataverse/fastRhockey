# **Load fastRhockey NHL play-by-play**

Helper that loads multiple seasons of pre-scraped NHL play-by-play data
(full version, including line changes and shifts) from the
[sportsdataverse-data](https://github.com/sportsdataverse/sportsdataverse-data)
releases either into memory or writes it into a database.

## Usage

``` r
load_nhl_pbp(
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

  Additional arguments passed to an underlying function that writes the
  season data into a database (used by
  [`update_nhl_db()`](https://fastRhockey.sportsdataverse.org/reference/update_nhl_db.md)).

- dbConnection:

  A `DBIConnection` object, as returned by
  [`DBI::dbConnect()`](https://dbi.r-dbi.org/reference/dbConnect.html)

- tablename:

  The name of the play by play data table within the database

## Value

A data frame (`fastRhockey_data`) with the following columns:

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
| period_seconds_remaining | integer | Seconds remaining in the period. |
| period_time_remaining | character | Time remaining in the period (MM:SS). |
| game_seconds | integer | Elapsed seconds in the game. |
| game_seconds_remaining | integer | Seconds remaining in regulation. |
| home_score | integer | Home team score after the event. |
| away_score | integer | Away team score after the event. |
| event_player_1_name | character | Name of the primary event player. |
| event_player_1_type | character | Role of the primary event player. |
| event_player_1_id | integer | Player id of the primary event player. |
| event_player_2_name | character | Name of the secondary event player. |
| event_player_2_type | character | Role of the secondary event player. |
| event_player_2_id | integer | Player id of the secondary event player. |
| event_player_3_name | character | Name of the tertiary event player. |
| event_player_3_type | character | Role of the tertiary event player. |
| event_player_3_id | integer | Player id of the tertiary event player. |
| event_goalie_name | character | Name of the goalie on the event. |
| event_goalie_id | integer | Player id of the goalie on the event. |
| penalty_severity | character | Severity of the penalty. |
| penalty_minutes | integer | Penalty minutes assessed. |
| strength_state | character | Strength state (e.g. 5v5, 5v4). |
| strength_code | character | Coded strength state. |
| strength | character | Strength description. |
| empty_net | logical | Whether the net was empty. |
| extra_attacker | logical | Whether an extra attacker was on the ice. |
| x | integer | Raw x-coordinate of the event. |
| y | integer | Raw y-coordinate of the event. |
| x_fixed | integer | Side-adjusted x-coordinate. |
| y_fixed | integer | Side-adjusted y-coordinate. |
| shot_distance | numeric | Distance of the shot from the net. |
| shot_angle | numeric | Angle of the shot relative to the net. |
| home_skaters | integer | Number of home skaters on the ice. |
| away_skaters | integer | Number of away skaters on the ice. |
| home_on_1 | character | Name of home skater 1 on the ice. |
| home_on_2 | character | Name of home skater 2 on the ice. |
| home_on_3 | character | Name of home skater 3 on the ice. |
| home_on_4 | character | Name of home skater 4 on the ice. |
| home_on_5 | character | Name of home skater 5 on the ice. |
| home_on_6 | character | Name of home skater 6 on the ice. |
| home_on_7 | character | Name of home skater 7 on the ice. |
| away_on_1 | character | Name of away skater 1 on the ice. |
| away_on_2 | character | Name of away skater 2 on the ice. |
| away_on_3 | character | Name of away skater 3 on the ice. |
| away_on_4 | character | Name of away skater 4 on the ice. |
| away_on_5 | character | Name of away skater 5 on the ice. |
| away_on_6 | character | Name of away skater 6 on the ice. |
| away_on_7 | character | Name of away skater 7 on the ice. |
| home_goalie | character | Name of the home goalie on the ice. |
| away_goalie | character | Name of the away goalie on the ice. |
| num_on | integer | Number of players coming on (line change). |
| players_on | character | Names of players coming on. |
| num_off | integer | Number of players going off (line change). |
| players_off | character | Names of players going off. |
| game_id | integer | Unique game identifier. |
| season | integer | Season (concluding year, YYYY). |
| season_type | character | Season type (regular/playoffs). |
| home_abbr | character | Home team abbreviation. |
| away_abbr | character | Away team abbreviation. |
| event_idx | integer | Sequential event index within the game. |
| event_id | integer | NHL event identifier. |
| away_goalie_in | integer | Whether the away goalie is in net (1/0). |
| home_goalie_in | integer | Whether the home goalie is in net (1/0). |
| reason | character | Reason for the event (e.g. stoppage reason). |
| secondaryReason | character | Secondary reason for the event. |
| ids_on | character | Player ids coming on. |
| ids_off | character | Player ids going off. |
| home_on_1_id | integer | Player id of home skater 1 on the ice. |
| away_on_1_id | integer | Player id of away skater 1 on the ice. |
| home_on_2_id | integer | Player id of home skater 2 on the ice. |
| away_on_2_id | integer | Player id of away skater 2 on the ice. |
| home_on_3_id | integer | Player id of home skater 3 on the ice. |
| away_on_3_id | integer | Player id of away skater 3 on the ice. |
| home_on_4_id | integer | Player id of home skater 4 on the ice. |
| away_on_4_id | integer | Player id of away skater 4 on the ice. |
| home_on_5_id | integer | Player id of home skater 5 on the ice. |
| away_on_5_id | integer | Player id of away skater 5 on the ice. |
| home_on_6_id | integer | Player id of home skater 6 on the ice. |
| away_on_6_id | integer | Player id of away skater 6 on the ice. |
| home_on_7_id | integer | Player id of home skater 7 on the ice. |
| away_on_7_id | integer | Player id of away skater 7 on the ice. |
| home_goalie_id | integer | Player id of the home goalie on the ice. |
| away_goalie_id | integer | Player id of the away goalie on the ice. |
| xg | numeric | Expected goals value for the shot event. |

## Examples

``` r
# \donttest{
  try(load_nhl_pbp(2022))
#> ── NHL Play-by-Play (full, with shifts) from fastRhockey data repository ───────
#> ℹ Data updated: 2026-04-08 05:53:06 UTC
#> # A tibble: 975,777 × 92
#>    event_type   event secondary_type event_team_abbr event_team_type description
#>    <chr>        <chr> <chr>          <chr>           <chr>           <chr>      
#>  1 CHANGE       Chan… NA             NA              NA              ON: Jeff C…
#>  2 CHANGE       Chan… NA             NA              NA              ON: Ryan M…
#>  3 FACEOFF      Face… NA             PIT             away            Jeff Carte…
#>  4 PERIOD_START Peri… NA             NA              NA              Start of P…
#>  5 HIT          Hit   NA             TBL             home            Ondrej Pal…
#>  6 CHANGE       Chan… NA             NA              NA              ON: Marcus…
#>  7 CHANGE       Chan… NA             NA              NA              ON: Alex K…
#>  8 CHANGE       Chan… NA             NA              NA              ON: Jason …
#>  9 STOP         Stop… NA             NA              NA              Stoppage i…
#> 10 FACEOFF      Face… NA             TBL             home            Anthony Ci…
#> # ℹ 975,767 more rows
#> # ℹ 86 more variables: period <int>, period_type <chr>, period_time <chr>,
#> #   period_seconds <int>, period_seconds_remaining <int>,
#> #   period_time_remaining <chr>, game_seconds <int>,
#> #   game_seconds_remaining <int>, home_score <int>, away_score <int>,
#> #   event_player_1_name <chr>, event_player_1_type <chr>,
#> #   event_player_1_id <int>, event_player_2_name <chr>, …
# }
```
