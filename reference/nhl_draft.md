# **NHL Draft**

Returns information on the most recent NHL draft picks.

Uses the new NHL API endpoint at `api-web.nhle.com/v1/draft/picks/now`.

## Usage

``` r
nhl_draft()
```

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| round | integer | Draft round number. |
| pick_in_round | integer | Pick number within the round. |
| overall_pick | integer | Overall pick number in the draft. |
| team_id | integer | Unique team identifier of the drafting team. |
| team_abbrev | character | Drafting team abbreviation. |
| team_name | data.frame | Drafting team name (localized). |
| team_common_name | data.frame | Drafting team common (nickname) name (localized). |
| team_place_name_with_preposition | data.frame | Drafting team place name with preposition. |
| display_abbrev | data.frame | Drafting team display abbreviation. |
| team_logo_light | character | URL to the team's light-theme logo. |
| team_logo_dark | character | URL to the team's dark-theme logo. |
| team_pick_history | character | History of the team's picks at this slot. |
| first_name | data.frame | Drafted player's first name (localized). |
| last_name | data.frame | Drafted player's last name (localized). |
| position_code | character | Drafted player's position code. |
| country_code | character | Drafted player's country code. |
| height | integer | Drafted player's height in inches. |
| weight | integer | Drafted player's weight in pounds. |
| amateur_league | character | Drafted player's amateur league. |
| amateur_club_name | character | Drafted player's amateur club name. |

## Examples

``` r
# \donttest{
   try(nhl_draft())
#> ── NHL Draft Data from NHL.com ──────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-07-18 18:47:41 UTC
#> # A tibble: 32 × 20
#>    round pick_in_round overall_pick team_id team_abbrev team_name$default  $fr  
#>    <int>         <int>        <int>   <int> <chr>       <chr>              <chr>
#>  1     1             1            1      10 TOR         Toronto Maple Lea… Mapl…
#>  2     1             2            2      28 SJS         San Jose Sharks    Shar…
#>  3     1             3            3      23 VAN         Vancouver Canucks  Canu…
#>  4     1             4            4       7 BUF         Buffalo Sabres     Sabr…
#>  5     1             5            5       3 NYR         New York Rangers   Rang…
#>  6     1             6            6      20 CGY         Calgary Flames     Flam…
#>  7     1             7            7      55 SEA         Seattle Kraken     Krak…
#>  8     1             8            8      52 WPG         Winnipeg Jets      Jets…
#>  9     1             9            9      28 SJS         San Jose Sharks    Shar…
#> 10     1            10           10      18 NSH         Nashville Predato… Pred…
#> # ℹ 22 more rows
#> # ℹ 14 more variables: team_common_name <df[,2]>,
#> #   team_place_name_with_preposition <df[,2]>, display_abbrev <df[,1]>,
#> #   team_logo_light <chr>, team_logo_dark <chr>, team_pick_history <chr>,
#> #   first_name <df[,1]>, last_name <df[,1]>, position_code <chr>,
#> #   country_code <chr>, height <int>, weight <int>, amateur_league <chr>,
#> #   amateur_club_name <chr>
# }
```
