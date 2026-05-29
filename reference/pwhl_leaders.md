# **PWHL League Leaders**

Retrieves PWHL league leaders (top scorers or top goalies) for a season.

## Usage

``` r
pwhl_leaders(
  position = "skaters",
  season = most_recent_pwhl_season(),
  game_type = "regular",
  limit = 100
)
```

## Arguments

- position:

  Either "skaters" (default) or "goalies".

- season:

  Season (YYYY) to pull leaders from. Defaults to
  [`most_recent_pwhl_season()`](https://fastRhockey.sportsdataverse.org/reference/most_recent_pwhl_season.md).

- game_type:

  Game type: "regular" (default), "preseason", or "playoffs".

- limit:

  Maximum number of leaders to return. Default 100.

## Value

A data frame (`fastRhockey_data`) with the following columns, or NULL if
unavailable:

|  |  |  |
|----|----|----|
| col_name | types | description |
| player_id | character | Unique player identifier. |
| shortname | character | Player short name. |
| first_name | character | Player first name. |
| last_name | character | Player last name. |
| name | character | Player full name. |
| phonetic_name | character | Phonetic spelling of the player name. |
| active | character | Whether the player is active. |
| height | character | Player height. |
| weight | character | Player weight. |
| last_years_club | character | Player's club in the previous season. |
| age | character | Player age. |
| shoots | character | Handedness the player shoots. |
| position | character | Player position. |
| suspension_games_remaining | character | Suspension games remaining. |
| suspension_indefinite | character | Whether the suspension is indefinite. |
| rookie | character | Whether the player is a rookie. |
| veteran | character | Whether the player is a veteran. |
| draft_eligible | character | Whether the player is draft eligible. |
| jersey_number | character | Jersey number. |
| team_name | character | Team name. |
| team_code | character | Team abbreviation. |
| team_id | character | Unique team identifier. |
| division | character | Team division. |
| birthdate | character | Player birthdate. |
| birthdate_year | character | Player birth year. |
| hometown | character | Player hometown. |
| homeprov | character | Player home province/state. |
| homecntry | character | Player home country. |
| birthtown | character | Player birth town. |
| birthprov | character | Player birth province/state. |
| birthcntry | character | Player birth country. |
| hometownprov | character | Player hometown and province/state. |
| homeplace | character | Player home place description. |
| games_played | numeric | Games played. |
| game_winning_goals | character | Game-winning goals. |
| game_tieing_goals | character | Game-tying goals. |
| first_goals | character | First goals of a game. |
| insurance_goals | character | Insurance goals. |
| unassisted_goals | character | Unassisted goals. |
| empty_net_goals | character | Empty-net goals. |
| overtime_goals | character | Overtime goals. |
| ice_time | character | Total ice time. |
| ice_time_avg | character | Average ice time. |
| goals | numeric | Goals scored. |
| shots | numeric | Shots on goal. |
| loose_ball_recoveries | character | Loose ball recoveries. |
| caused_turnovers | character | Turnovers caused. |
| turnovers | character | Turnovers committed. |
| hits | character | Hits delivered. |
| shots_blocked_by_player | character | Shots blocked by the player. |
| ice_time_minutes_seconds | character | Ice time in minutes and seconds. |
| shooting_percentage | numeric | Shooting percentage. |
| assists | numeric | Assists. |
| points | numeric | Total points (goals + assists). |
| points_per_game | numeric | Points per game. |
| plus_minus | numeric | Plus/minus rating. |
| penalty_minutes | numeric | Penalty minutes. |
| penalty_minutes_per_game | character | Penalty minutes per game. |
| ice_time_per_game_avg | character | Average ice time per game. |
| hits_per_game_avg | character | Average hits per game. |
| minor_penalties | character | Minor penalties. |
| major_penalties | character | Major penalties. |
| power_play_goals | numeric | Power-play goals. |
| power_play_assists | numeric | Power-play assists. |
| power_play_points | character | Power-play points. |
| short_handed_goals | numeric | Short-handed goals. |
| short_handed_assists | numeric | Short-handed assists. |
| short_handed_points | character | Short-handed points. |
| shootout_goals | character | Shootout goals. |
| shootout_attempts | character | Shootout attempts. |
| shootout_winning_goals | character | Shootout game-winning goals. |
| shootout_games_played | character | Games played that went to a shootout. |
| faceoff_attempts | character | Faceoff attempts. |
| faceoff_wins | character | Faceoffs won. |
| faceoff_pct | character | Faceoff win percentage. |
| faceoff_wa | character | Faceoff wins-to-attempts metric. |
| shots_on | character | Shots on goal count. |
| shootout_percentage | character | Shootout scoring percentage. |
| latest_team_id | character | Most recent team identifier. |
| num_teams | character | Number of teams the player has played for. |
| logo | character | URL to the team logo. |
| rank | numeric | Leader rank. |
| player_page_link | character | URL to the player page. |
| namelink | character | HTML link for the player name. |
| teamlink | character | HTML link for the team. |
| photo | character | URL to the player photo. |
| team_breakdown | character | Per-team statistical breakdown. |
| is_total | character | Whether the row is a season total. |

## Examples

``` r
# \donttest{
  try(pwhl_leaders(position = "skaters", season = 2025))
#> ── PWHL Leaders - skaters ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:37:48 UTC
#> # A tibble: 107 × 88
#>    player_id shortname    first_name last_name name  phonetic_name active height
#>    <chr>     <chr>        <chr>      <chr>     <chr> <chr>         <chr>  <chr> 
#>  1 13        H. Knight    Hilary     Knight    Hila… "HILL-AH-REE… 1      "5'11"
#>  2 205       S. Fillier   Sarah      Fillier   Sara… "SAIR-AH FIH… 1      "5'5" 
#>  3 63        D. Watts     Daryl      Watts     Dary… "DAIR-uhl WA… 1      "5'6\…
#>  4 31        M. Poulin    Marie-Phi… Poulin    Mari… ""            1      "5'7" 
#>  5 20        K. Coyne Sc… Kendall    Coyne Sc… Kend… "KEHN-duhl K… 1      "5'2" 
#>  6 89        H. Miller    Hannah     Miller    Hann… "HAN-uh MIH-… 1      "5'9\…
#>  7 36        J. Eldridge  Jessie     Eldridge  Jess… "jeh-see EHL… 1      "5'9\…
#>  8 161       T. Vanišová  Tereza     Vanišová  Tere… " TAH-ree-zu… 1      "5'7" 
#>  9 32        L. Stacey    Laura      Stacey    Laur… "STAY-see"    1      "5'10…
#> 10 21        T. Heise     Taylor     Heise     Tayl… "TAY-luhr HI… 1      "5'10"
#> # ℹ 97 more rows
#> # ℹ 80 more variables: weight <chr>, last_years_club <chr>, age <chr>,
#> #   shoots <chr>, position <chr>, suspension_games_remaining <chr>,
#> #   suspension_indefinite <chr>, rookie <chr>, veteran <chr>,
#> #   draft_eligible <chr>, jersey_number <chr>, team_name <chr>,
#> #   team_code <chr>, team_id <chr>, division <chr>, birthdate <chr>,
#> #   birthdate_year <chr>, hometown <chr>, homeprov <chr>, homecntry <chr>, …
  try(pwhl_leaders(position = "goalies", season = 2025))
#> ── PWHL Leaders - goalies ───────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 17:37:48 UTC
#> # A tibble: 16 × 84
#>    player_id shortname    rookie first_name last_name name  phonetic_name active
#>    <chr>     <chr>        <chr>  <chr>      <chr>     <chr> <chr>         <chr> 
#>  1 134       C. Jackson   0      Carly      Jackson   Carl… "CAR-lee JAK… 1     
#>  2 186       K. Peslarová 0      Klára      Peslarová Klár… "KLAH-ruh PE… 1     
#>  3 28        A. Desbiens  0      Ann-Renée  Desbiens  Ann-… ""            1     
#>  4 123       M. Rooney    0      Maddie     Rooney    Madd… "MAD-dee ROO… 1     
#>  5 222       G. Philips   1      Gwyneth    Philips   Gwyn… "GWIH-nihth … 1     
#>  6 228       K. Osborne   1      Kayle      Osborne   Kayl… "KAY-lee AWZ… 1     
#>  7 64        K. Campbell  0      Kristen    Campbell  Kris… "KRIHS-tehn … 1     
#>  8 211       R. Kirk      1      Raygan     Kirk      Rayg… "ray-GEHN ku… 1     
#>  9 6         A. Frankel   0      Aerin      Frankel   Aeri… "AIR-IN FRAN… 1     
#> 10 85        E. Chuli     0      Elaine     Chuli     Elai… ""            1     
#> 11 155       C. Schroeder 0      Corinne    Schroeder Cori… "kohr-een SH… 1     
#> 12 22        N. Hensley   0      Nicole     Hensley   Nico… "NIHK-ohl HE… 1     
#> 13 59        E. Maschmey… 0      Emerance   Maschmey… Emer… "EH-muhr-ehn… 1     
#> 14 19        E. Söderberg 0      Emma       Söderberg Emma… "EH-mah SOH-… 1     
#> 15 41        A. Levy      0      Abbey      Levy      Abbe… "AH-BEE LEE-… 1     
#> 16 193       L. Morgan    0      Lucy       Morgan    Lucy… ""            1     
#> # ℹ 76 more variables: height <chr>, weight <chr>, position <chr>,
#> #   suspension_games_remaining <chr>, suspension_indefinite <chr>,
#> #   start_date <chr>, veteran <chr>, draft_eligible <chr>, jersey_number <chr>,
#> #   shoots <chr>, catches <chr>, team_name <chr>, team_code <chr>,
#> #   team_id <chr>, division <chr>, birthdate <chr>, birthdate_year <chr>,
#> #   age <chr>, hometown <chr>, homeprov <chr>, homecntry <chr>,
#> #   birthtown <chr>, birthprov <chr>, birthcntry <chr>, hometownprov <chr>, …
# }
```
