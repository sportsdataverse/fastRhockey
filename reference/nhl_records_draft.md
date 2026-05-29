# **NHL Records - Draft Listing**

Returns the NHL Entry Draft listing from the NHL Records API
(`https://records.nhl.com/site/api/draft`). Supports Cayenne filtering
and pagination.

## Usage

``` r
nhl_records_draft(cayenne_exp = NULL, limit = NULL, start = NULL)
```

## Arguments

- cayenne_exp:

  Optional Cayenne filter expression string (e.g. `"draftYear=2015"`).

- limit:

  Optional integer page size.

- start:

  Optional integer pagination offset.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                      |           |                                        |
|----------------------|-----------|----------------------------------------|
| col_name             | types     | description                            |
| id                   | integer   | Unique draft record identifier.        |
| age_in_days          | integer   | Player age in days.                    |
| age_in_days_for_year | integer   | Player age in days for the draft year. |
| age_in_years         | integer   | Player age in years.                   |
| amateur_club_name    | character | Amateur club the player played for.    |
| amateur_league       | character | Amateur league the player played in.   |
| birth_date           | character | Player birth date.                     |
| birth_place          | character | Player birth place.                    |
| country_code         | character | Player country code.                   |
| cs_player_id         | integer   | Central Scouting player identifier.    |
| draft_date           | character | Date the player was drafted.           |
| draft_master_id      | integer   | Draft master record identifier.        |
| draft_year           | integer   | Year of the draft.                     |
| drafted_by_team_id   | integer   | Identifier of the drafting team.       |
| first_name           | character | Player first name.                     |
| height               | integer   | Player height in inches.               |
| last_name            | character | Player last name.                      |
| notes                | logical   | Notes flag for the pick.               |
| overall_pick_number  | integer   | Overall pick number in the draft.      |
| pick_in_round        | integer   | Pick number within the round.          |
| player_id            | integer   | Unique player identifier.              |
| player_name          | character | Player full name.                      |
| position             | character | Player position.                       |
| removed_outright     | character | Removed-outright indicator.            |
| removed_outright_why | logical   | Reason the pick was removed outright.  |
| round_number         | integer   | Draft round number.                    |
| shoots_catches       | character | Player shooting/catching hand.         |
| supplemental_draft   | character | Supplemental draft indicator.          |
| team_pick_history    | character | History of the team's pick.            |
| tri_code             | character | Three-letter team code.                |
| weight               | integer   | Player weight in pounds.               |

## Examples

``` r
# \donttest{
  try(nhl_records_draft(cayenne_exp = "draftYear=2020", limit = 5))
#> ── NHL Records Draft ────────────────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 16:26:45 UTC
#> # A tibble: 5 × 31
#>      id age_in_days age_in_days_for_year age_in_years amateur_club_name
#>   <int>       <int>                <int>        <int> <chr>            
#> 1 18392        6935                  361           18 Rimouski         
#> 2 18393        6623                   48           18 Sudbury          
#> 3 18394        6839                  265           18 Mannheim         
#> 4 18395        6767                  192           18 Frolunda         
#> 5 18396        6665                   90           18 USA U-18         
#> # ℹ 26 more variables: amateur_league <chr>, birth_date <chr>,
#> #   birth_place <chr>, country_code <chr>, cs_player_id <int>,
#> #   draft_date <chr>, draft_master_id <int>, draft_year <int>,
#> #   drafted_by_team_id <int>, first_name <chr>, height <int>, last_name <chr>,
#> #   notes <lgl>, overall_pick_number <int>, pick_in_round <int>,
#> #   player_id <int>, player_name <chr>, position <chr>, removed_outright <chr>,
#> #   removed_outright_why <lgl>, round_number <int>, shoots_catches <chr>, …
# }
```
