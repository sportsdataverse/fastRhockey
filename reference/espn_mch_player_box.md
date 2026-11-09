# **Get ESPN MCH / WCH Player Box Score**

**Get ESPN MCH / WCH Player Box Score**

## Usage

``` r
espn_mch_player_box(game_id, ...)

espn_wch_player_box(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble, one row per player appearance (skaters and
goalies bound; columns not kept for a position group are `NA`):

|  |  |  |
|----|----|----|
| col_name | type | description |
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
| plus_minus | character | Plus/minus rating (skaters). |
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

## Details

League slug `mens-college-hockey`. Parsed from the site-v2 `summary`
payload by the same helper as
[`espn_nhl_player_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_box.md);
columns verified against a captured mch summary body.

`espn_wch_player_box()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_player_box(game_id = "401717648"))
#> ── MENS-COLLEGE-HOCKEY Player Box Score data from ESPN.com ─────────────────────
#> ℹ Data updated: 2026-11-09 11:49:20 UTC
#> # A tibble: 41 × 53
#>    athlete_id athlete_display_name athlete_short_name athlete_last_name
#>    <chr>      <chr>                <chr>              <chr>            
#>  1 5136656    Tristan Amonte       T. Amonte          Amonte           
#>  2 5216936    Kamil Bednarik       K. Bednarik        Bednarik         
#>  3 5137145    Matt Copponi         M. Copponi         Copponi          
#>  4 5216888    Cole Eiserman        C. Eiserman        Eiserman         
#>  5 5080227    Ryan Greene          R. Greene          Greene           
#>  6 5188526    Jack Harvey          J. Harvey          Harvey           
#>  7 4996314    Jack Hughes          J. Hughes          Hughes           
#>  8 5136660    Quinn Hutson         Q. Hutson          Hutson           
#>  9 5136662    Devin Kaplan         D. Kaplan          Kaplan           
#> 10 5103873    Shane Lachance       S. Lachance        Lachance         
#> # ℹ 31 more rows
#> # ℹ 49 more variables: athlete_jersey <chr>, athlete_headshot_href <chr>,
#> #   athlete_position_name <chr>, athlete_position_abbreviation <chr>,
#> #   athlete_active <lgl>, athlete_scratched <lgl>, stat_group <chr>,
#> #   team_id <chr>, team_abbreviation <chr>, team_display_name <chr>,
#> #   home_away <chr>, team_score <chr>, team_winner <lgl>,
#> #   opponent_team_id <chr>, opponent_team_abbreviation <chr>, game_id <chr>, …
# }
# \donttest{
  try(espn_wch_player_box(game_id = "401762970"))
#> ── WOMENS-COLLEGE-HOCKEY Player Box Score data from ESPN.com ───────────────────
#> ℹ Data updated: 2026-11-09 11:49:20 UTC
#> # A tibble: 40 × 53
#>    athlete_id athlete_display_name athlete_short_name athlete_last_name
#>    <chr>      <chr>                <chr>              <chr>            
#>  1 5263060    Greta Brezenski      G. Brezenski       Brezenski        
#>  2 5263072    Sena Catterall       S. Catterall       Catterall        
#>  3 5263074    Madison Chantler     M. Chantler        Chantler         
#>  4 5263071    Anne Cherkowski      A. Cherkowski      Cherkowski       
#>  5 5263057    Caroline Goffredo    C. Goffredo        Goffredo         
#>  6 5263058    Jenna Goodwin        J. Goodwin         Goodwin          
#>  7 5263055    Rhea Hicks           R. Hicks           Hicks            
#>  8 5263056    Baylee Kirwan        B. Kirwan          Kirwan           
#>  9 5263075    Shelby Laidlaw       S. Laidlaw         Laidlaw          
#> 10 5263062    Kirstyn McQuigge     K. McQuigge        McQuigge         
#> # ℹ 30 more rows
#> # ℹ 49 more variables: athlete_jersey <chr>, athlete_headshot_href <chr>,
#> #   athlete_position_name <chr>, athlete_position_abbreviation <chr>,
#> #   athlete_active <lgl>, athlete_scratched <lgl>, stat_group <chr>,
#> #   team_id <chr>, team_abbreviation <chr>, team_display_name <chr>,
#> #   home_away <chr>, team_score <chr>, team_winner <lgl>,
#> #   opponent_team_id <chr>, opponent_team_abbreviation <chr>, game_id <chr>, …
# }
```
