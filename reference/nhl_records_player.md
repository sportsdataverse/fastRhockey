# **NHL Records - Player Listing**

Returns the player listing from the NHL Records API
(`https://records.nhl.com/site/api/player`). Optionally filter to a
single player via `player_id` (switches resource to
`player/{player_id}`) or pass an arbitrary Cayenne expression.

## Usage

``` r
nhl_records_player(player_id = NULL, cayenne_exp = NULL)
```

## Arguments

- player_id:

  Optional integer player ID. If supplied, the resource becomes
  `player/{player_id}`.

- cayenne_exp:

  Optional Cayenne filter expression string. Ignored when `player_id` is
  supplied (single-player endpoint).

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique player identifier. |
| accrued_seasons | integer | Number of accrued NHL seasons. |
| add_names | character | Additional names for the player. |
| age_sign_waiver | integer | Age at which the player signed a waiver. |
| age_signel_fa | integer | Age at which the player signed as a free agent. |
| alert | character | Alert flag or note. |
| birth_city | character | Birth city of the player. |
| birth_country | character | Birth country of the player. |
| birth_date | character | Birth date of the player. |
| birth_state_province | character | Birth state or province of the player. |
| career_team_id | integer | Primary career team identifier. |
| central_registry_position | character | Position per the central registry. |
| club_elec_arb | character | Club election to arbitration indicator. |
| current_team_id | integer | Current team identifier. |
| date_of_death | character | Date of death, if applicable. |
| dda_id | integer | DDA identifier. |
| deceased | logical | Whether the player is deceased. |
| ep_player_id | integer | EliteProspects player identifier. |
| fa_group_after_season | logical | Free agent group after the season. |
| first_name | character | First name of the player. |
| first_signed_by_team_id | integer | Team that first signed the player. |
| free_agent_group | character | Free agent group classification. |
| full_name | character | Full name of the player. |
| group5election | character | Group 5 free agency election indicator. |
| group5seasons_earned | integer | Group 5 seasons earned. |
| group6proration | logical | Group 6 proration indicator. |
| group6seasons_earned | integer | Group 6 seasons earned. |
| groups_earned_thru_season | integer | Season through which groups were earned. |
| height | integer | Player height in inches. |
| hof_induction_year | integer | Hockey Hall of Fame induction year. |
| home_town | character | Home town of the player. |
| iihf_hof_induction_year | integer | IIHF Hall of Fame induction year. |
| in_hockey_hof | logical | Whether the player is in the Hockey Hall of Fame. |
| in_iihf_hof | integer | Whether the player is in the IIHF Hall of Fame. |
| in_top100alltime | logical | Whether the player is in the all-time top 100. |
| in_us_hockey_hof | logical | Whether the player is in the US Hockey Hall of Fame. |
| is_defected | character | Whether the player defected. |
| is_deleted | character | Whether the record is deleted. |
| is_junior | character | Whether the player is a junior. |
| is_retired | logical | Whether the player is retired. |
| is_rookie | character | Whether the player is a rookie. |
| is_suspended | character | Whether the player is suspended. |
| last_amateur_league_id | integer | Last amateur league identifier. |
| last_amateur_team_id | integer | Last amateur team identifier. |
| last_nhl_team_id | integer | Last NHL team identifier. |
| last_name | character | Last name of the player. |
| loan_cap_exception | character | Loan cap exception indicator. |
| long_term_injury | character | Long-term injury indicator. |
| message | character | Status message. |
| middle_name | character | Middle name of the player. |
| nationality | character | Nationality of the player. |
| nhl_experience | integer | Years of NHL experience. |
| on_roster | character | Whether the player is on a roster. |
| platform_year | integer | Platform year for contract purposes. |
| position | character | Player position. |
| pr_name | character | Public relations display name. |
| pr_stat | integer | Public relations status code. |
| pro_year_reduction | integer | Professional year reduction. |
| reentry_waivers | character | Re-entry waivers indicator. |
| roster_special_code | character | Roster special status code. |
| salary_arbitration_exp | integer | Salary arbitration experience. |
| shoots_catches | character | Handedness (shoots or catches). |
| sweater_number | integer | Sweater number worn by the player. |
| update_timestamp | character | Timestamp of the last record update. |
| us_hof_induction_year | integer | US Hockey Hall of Fame induction year. |
| vet_cap_excptn | character | Veteran cap exception indicator. |
| waiver_amount | integer | Waiver amount. |
| waiver_draft | character | Waiver draft indicator. |
| waiver_status | character | Waiver status. |
| weight | integer | Player weight in pounds. |
| years_pro | integer | Number of professional years played. |

## Examples

``` r
# \donttest{
  try(nhl_records_player())
#> ── NHL Records Player ───────────────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-06 07:49:41 UTC
#> # A tibble: 23,576 × 16
#>         id birth_city       birth_country birth_date birth_state_province
#>      <int> <chr>            <chr>         <chr>      <chr>               
#>  1 8444850 Kenora           CAN           1906-04-28 ON                  
#>  2 8444851 Haileybury       CAN           1897-07-25 ON                  
#>  3 8444852 Gowganda         CAN           1912-04-18 ON                  
#>  4 8444853 Moose Jaw        CAN           1932-11-27 SK                  
#>  5 8444854 Winnipeg         CAN           1930-02-04 MB                  
#>  6 8444855 Sault Ste. Marie USA           1900-05-28 MI                  
#>  7 8444856 Detroit          USA           1944-12-25 MI                  
#>  8 8444857 Melville         CAN           1918-02-22 SK                  
#>  9 8444858 Lamont           CAN           1932-09-07 AB                  
#> 10 8444859 Toronto          CAN           1949-05-15 ON                  
#> # ℹ 23,566 more rows
#> # ℹ 11 more variables: current_team_id <int>, first_name <chr>,
#> #   full_name <chr>, height <int>, in_top100alltime <lgl>, last_name <chr>,
#> #   on_roster <chr>, position <chr>, sweater_number <int>, weight <int>,
#> #   years_pro <int>
# }
```
