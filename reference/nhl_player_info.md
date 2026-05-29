# **NHL Player Info**

Returns biographical and career information for an NHL player. Uses the
NHL API (`api-web.nhle.com`).

## Usage

``` r
nhl_player_info(player_id)
```

## Arguments

- player_id:

  Integer player ID (e.g., 8476899)

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                 |           |                                         |
|-----------------|-----------|-----------------------------------------|
| col_name        | types     | description                             |
| player_id       | integer   | Unique player identifier.               |
| first_name      | character | Player first name.                      |
| last_name       | character | Player last name.                       |
| full_name       | character | Player full name.                       |
| team_abbr       | character | Current team abbreviation.              |
| team_name       | character | Current team name.                      |
| sweater_number  | integer   | Player sweater (jersey) number.         |
| position        | character | Player position.                        |
| shoots_catches  | character | Handedness (shoots or catches).         |
| height_inches   | integer   | Player height in inches.                |
| weight_pounds   | integer   | Player weight in pounds.                |
| birth_date      | character | Player date of birth.                   |
| birth_city      | character | City of birth.                          |
| birth_state     | character | State or province of birth.             |
| birth_country   | character | Country of birth.                       |
| draft_year      | integer   | Year the player was drafted.            |
| draft_round     | integer   | Draft round.                            |
| draft_pick      | integer   | Draft pick within the round.            |
| draft_overall   | integer   | Overall draft selection number.         |
| draft_team_abbr | character | Abbreviation of the drafting team.      |
| is_active       | logical   | Whether the player is currently active. |
| headshot_url    | character | URL of the player headshot image.       |

## Examples

``` r
# \donttest{
  try(nhl_player_info(player_id = 8476899))
#> ── NHL Player Info ──────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:06:49 UTC
#> # A tibble: 1 × 22
#>   player_id first_name last_name full_name   team_abbr team_name  sweater_number
#>       <int> <chr>      <chr>     <chr>       <chr>     <chr>               <int>
#> 1   8476899 Matt       Murray    Matt Murray SEA       Seattle K…             30
#> # ℹ 15 more variables: position <chr>, shoots_catches <chr>,
#> #   height_inches <int>, weight_pounds <int>, birth_date <chr>,
#> #   birth_city <chr>, birth_state <chr>, birth_country <chr>, draft_year <int>,
#> #   draft_round <int>, draft_pick <int>, draft_overall <int>,
#> #   draft_team_abbr <chr>, is_active <lgl>, headshot_url <chr>
# }
```
