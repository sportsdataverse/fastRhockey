# **NHL Records - Players by Team**

Returns every player who has suited up for a given team via the NHL
Records API
(`https://records.nhl.com/site/api/player/byTeam/{team_id}`).

## Usage

``` r
nhl_records_player_byteam(team_id, cayenne_exp = NULL)
```

## Arguments

- team_id:

  Integer team ID (required).

- cayenne_exp:

  Optional Cayenne filter expression string.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|  |  |  |
|----|----|----|
| col_name | types | description |
| id | integer | Unique player identifier. |
| accrued_seasons | integer | Number of accrued NHL seasons. |
| add_names | logical | Additional names for the player. |
| age_sign_waiver | integer | Age at which the player signed a waiver. |
| age_signel_fa | integer | Age at which the player signed as a free agent. |
| alert | character | Alert flag or note. |
| birth_city | character | Birth city of the player. |
| birth_country | character | Birth country of the player. |
| birth_date | character | Birth date of the player. |
| birth_state_province | character | Birth state or province of the player. |
| career_team_id | logical | Primary career team identifier. |
| central_registry_position | character | Position per the central registry. |
| club_elec_arb | character | Club election to arbitration indicator. |
| current_team_id | integer | Current team identifier. |
| date_of_death | logical | Date of death, if applicable. |
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
| hof_induction_year | logical | Hockey Hall of Fame induction year. |
| home_town | character | Home town of the player. |
| iihf_hof_induction_year | logical | IIHF Hall of Fame induction year. |
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
| roster_special_code | logical | Roster special status code. |
| salary_arbitration_exp | integer | Salary arbitration experience. |
| shoots_catches | character | Handedness (shoots or catches). |
| sweater_number | integer | Sweater number worn by the player. |
| update_timestamp | character | Timestamp of the last record update. |
| us_hof_induction_year | logical | US Hockey Hall of Fame induction year. |
| vet_cap_excptn | character | Veteran cap exception indicator. |
| waiver_amount | integer | Waiver amount. |
| waiver_draft | character | Waiver draft indicator. |
| waiver_status | character | Waiver status. |
| weight | integer | Player weight in pounds. |
| years_pro | integer | Number of professional years played. |

## Examples

``` r
# \donttest{
  try(nhl_records_player_byteam(team_id = 10))
#> ── NHL Records Player by Team ───────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-04 00:52:23 UTC
#> # A tibble: 80 × 16
#>         id birth_city   birth_country birth_date birth_state_province
#>      <int> <chr>        <chr>         <chr>      <chr>               
#>  1 8475166 Mississauga  CAN           1990-09-20 ON                  
#>  2 8477425 Buffalo      USA           1995-09-13 NY                  
#>  3 8480893 Barnaul      RUS           2000-07-21 NA                  
#>  4 8483546 Beaconsfield CAN           1998-03-16 QC                  
#>  5 8483565 Washington   USA           1998-05-12 MI                  
#>  6 8477503 Winnipeg     CAN           1995-03-02 MB                  
#>  7 8478007 Riga         LVA           1994-04-13 NA                  
#>  8 8479026 Moncton      CAN           1997-01-25 NB                  
#>  9 8479528 Lipetsk      RUS           1997-04-18 NA                  
#> 10 8479543 Toronto      CAN           1998-03-13 ON                  
#> # ℹ 70 more rows
#> # ℹ 11 more variables: current_team_id <int>, first_name <chr>,
#> #   full_name <chr>, height <int>, in_top100alltime <lgl>, last_name <chr>,
#> #   on_roster <chr>, position <chr>, sweater_number <int>, weight <int>,
#> #   years_pro <int>
# }
```
