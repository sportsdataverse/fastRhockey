# **Get ESPN NHL Player Box Score**

**Get ESPN NHL Player Box Score**

## Usage

``` r
espn_nhl_player_box(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per player appearance. Skater
and goalie rows are combined via
[`dplyr::bind_rows`](https://dplyr.tidyverse.org/reference/bind_rows.html);
columns not applicable to a position group are `NA`. Key columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| athlete_id | character | ESPN athlete identifier. |
| athlete_display_name | character | Athlete full display name. |
| athlete_short_name | character | Athlete short name. |
| athlete_last_name | character | Athlete last name. |
| athlete_jersey | character | Jersey number. |
| athlete_headshot_href | character | Headshot image URL. |
| athlete_position_name | character | Position name. |
| athlete_position_abbreviation | character | Position abbreviation. |
| athlete_active | logical | Whether the athlete was active. |
| athlete_scratched | logical | Whether the athlete was scratched. |
| stat_group | character | Stat group name ("forwards","defenses","goalies"). |
| team_id | character | ESPN team identifier. |
| team_abbreviation | character | Team abbreviation. |
| team_display_name | character | Team display name. |
| home_away | character | "home" or "away". |
| team_score | character | Team final score. |
| team_winner | logical | Whether the team won. |
| opponent_team_id | character | Opponent team identifier. |
| opponent_team_abbreviation | character | Opponent team abbreviation. |
| game_id | character | ESPN event identifier (echoed from arg). |
| season | integer | Season end-year. |
| season_type | integer | Season type code. |
| game_date | character | Game date-time (ISO 8601). |
| bs | character | Blocked shots (skaters). |
| ht | character | Hits (skaters). |
| tk | character | Takeaways (skaters). |
| plus_minus | integer | Plus/minus rating (skaters). |
| toi | character | Time on ice (skaters, MM:SS). |
| pptoi | character | Power play time on ice (skaters, MM:SS). |
| shtoi | character | Short-handed time on ice (skaters, MM:SS). |
| estoi | character | Even-strength time on ice (skaters, MM:SS). |
| shft | character | Total shifts (skaters). |
| g | character | Goals (skaters). |
| ytdg | character | Year-to-date goals (skaters/goalies). |
| a | character | Assists (skaters). |
| s | character | Shots (skaters). |
| sm | character | Missed shots (skaters). |
| sog | character | Shots on goal (skaters). |
| fw | character | Faceoffs won (skaters). |
| fl | character | Faceoffs lost (skaters). |
| fo_percent | character | Faceoff percentage (skaters). |
| gv | character | Giveaways (skaters). |
| pn | character | Penalties taken (skaters). |
| pim | character | Penalty minutes (skaters/goalies). |
| ga | character | Goals against (goalies). |
| sa | character | Shots against (goalies). |
| sos | character | Shootout saves (goalies). |
| sosa | character | Shootout shots against (goalies). |
| sv | character | Saves (goalies). |
| sv_percent | character | Save percentage (goalies). |
| essv | character | Even-strength saves (goalies). |
| ppsv | character | Power play saves (goalies). |
| shsv | character | Short-handed saves (goalies). |

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_player_box(game_id = "401688263"))
#> ── NHL Player Box Score data from ESPN.com ──────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 19:34:41 UTC
#> # A tibble: 38 × 53
#>    athlete_id athlete_display_name athlete_short_name athlete_last_name
#>    <chr>      <chr>                <chr>              <chr>            
#>  1 5149125    Connor Bedard        C. Bedard          Bedard           
#>  2 3042056    Tyler Bertuzzi       T. Bertuzzi        Bertuzzi         
#>  3 4874940    Colton Dach          C. Dach            Dach             
#>  4 3042062    Jason Dickinson      J. Dickinson       Dickinson        
#>  5 3115033    Ryan Donato          R. Donato          Donato           
#>  6 3535       Nick Foligno         N. Foligno         Foligno          
#>  7 5428       Taylor Hall          T. Hall            Hall             
#>  8 4392541    Philipp Kurashev     P. Kurashev        Kurashev         
#>  9 3853       Pat Maroon           P. Maroon          Maroon           
#> 10 4422415    Ilya Mikheyev        I. Mikheyev        Mikheyev         
#> # ℹ 28 more rows
#> # ℹ 49 more variables: athlete_jersey <chr>, athlete_headshot_href <chr>,
#> #   athlete_position_name <chr>, athlete_position_abbreviation <chr>,
#> #   athlete_active <lgl>, athlete_scratched <lgl>, stat_group <chr>,
#> #   team_id <chr>, team_abbreviation <chr>, team_display_name <chr>,
#> #   home_away <chr>, team_score <chr>, team_winner <lgl>,
#> #   opponent_team_id <chr>, opponent_team_abbreviation <chr>, game_id <chr>, …
# }
```
