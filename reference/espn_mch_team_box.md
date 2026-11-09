# **Get ESPN MCH / WCH Team Box Score**

**Get ESPN MCH / WCH Team Box Score**

## Usage

``` r
espn_mch_team_box(game_id, ...)

espn_wch_team_box(game_id, ...)
```

## Arguments

- game_id:

  ESPN event identifier (character or numeric).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble, one row per team (2 rows):

|  |  |  |
|----|----|----|
| col_name | type | description |
| team_id | character | ESPN team identifier. |
| team_uid | character | ESPN team uid. |
| team_slug | character | Team URL slug. |
| team_location | character | Team city/location. |
| team_name | character | Team name (mascot). |
| team_abbreviation | character | Team abbreviation. |
| team_display_name | character | Team display name. |
| team_short_display_name | character | Team short display name. |
| team_color | character | Team primary color hex. |
| team_alternate_color | character | Team alternate color hex. |
| team_logo | character | Team logo URL. |
| home_away | character | "home" or "away". |
| game_id | character | ESPN event identifier (echoed from arg). |
| season | integer | Season end-year. |
| season_type | integer | Season type code. |
| game_date | character | Game date-time (ISO 8601). |
| blocked_shots | character | Total blocked shots. |
| hits | character | Total hits. |
| takeaways | character | Total takeaways. |
| shots_total | character | Total shots on goal. |
| power_play_goals | character | Power play goals. |
| power_play_opportunities | character | Power play opportunities. |
| power_play_pct | character | Power play percentage. |
| short_handed_goals | character | Short-handed goals. |
| shootout_goals | character | Shootout goals. |
| faceoffs_won | character | Faceoffs won. |
| faceoff_percent | character | Faceoff win percentage. |
| giveaways | character | Giveaways. |
| penalties | character | Penalty count. |
| penalty_minutes | character | Penalty minutes. |

## Details

League slug `mens-college-hockey`. Parsed from the site-v2 `summary`
payload by the same helper as
[`espn_nhl_team_box()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_box.md);
columns verified against a captured mch summary body.

`espn_wch_team_box()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team_box(game_id = "401717648"))
#> ── MENS-COLLEGE-HOCKEY Team Box Score data from ESPN.com ── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:35 UTC
#> # A tibble: 2 × 30
#>   team_id team_uid         team_slug   team_location team_name team_abbreviation
#>   <chr>   <chr>            <chr>       <chr>         <chr>     <chr>            
#> 1 104     s:70~l:91~t:104  boston-uni… Boston Unive… Terriers  BU               
#> 2 2711    s:70~l:91~t:2711 western-mi… Western Mich… Broncos   WMU              
#> # ℹ 24 more variables: team_display_name <chr>, team_short_display_name <chr>,
#> #   team_color <chr>, team_alternate_color <chr>, team_logo <chr>,
#> #   home_away <chr>, game_id <chr>, season <int>, season_type <int>,
#> #   game_date <chr>, blocked_shots <chr>, hits <chr>, takeaways <chr>,
#> #   shots_total <chr>, power_play_goals <chr>, power_play_opportunities <chr>,
#> #   power_play_pct <chr>, short_handed_goals <chr>, shootout_goals <chr>,
#> #   faceoffs_won <chr>, faceoff_percent <chr>, giveaways <chr>, …
# }
# \donttest{
  try(espn_wch_team_box(game_id = "401762970"))
#> ── WOMENS-COLLEGE-HOCKEY Team Box Score data from ESPN.com ─────────────────────
#> ℹ Data updated: 2026-11-09 11:49:36 UTC
#> # A tibble: 2 × 30
#>   team_id team_uid         team_slug   team_location team_name team_abbreviation
#>   <chr>   <chr>            <chr>       <chr>         <chr>     <chr>            
#> 1 2137    s:70~l:92~t:2137 clarkson-g… Clarkson      Golden K… CLAR             
#> 2 275     s:70~l:92~t:275  wisconsin-… Wisconsin     Badgers   WIS              
#> # ℹ 24 more variables: team_display_name <chr>, team_short_display_name <chr>,
#> #   team_color <chr>, team_alternate_color <chr>, team_logo <chr>,
#> #   home_away <chr>, game_id <chr>, season <int>, season_type <int>,
#> #   game_date <chr>, blocked_shots <chr>, hits <chr>, takeaways <chr>,
#> #   shots_total <chr>, power_play_goals <chr>, power_play_opportunities <chr>,
#> #   power_play_pct <chr>, short_handed_goals <chr>, shootout_goals <chr>,
#> #   faceoffs_won <chr>, faceoff_percent <chr>, giveaways <chr>, …
# }
```
