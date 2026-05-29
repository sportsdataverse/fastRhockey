# **NHL Records - Draft Prospect Listing**

Returns the draft prospect listing from the NHL Records API
(`https://records.nhl.com/site/api/draft-prospect`). Optionally filter
to a single prospect via `prospect_id` (switches resource to
`draft-prospect/{prospect_id}`).

## Usage

``` r
nhl_records_draft_prospect(prospect_id = NULL, cayenne_exp = NULL)
```

## Arguments

- prospect_id:

  Optional integer prospect ID. If supplied, the resource becomes
  `draft-prospect/{prospect_id}`.

- cayenne_exp:

  Optional Cayenne filter expression string. Ignored when `prospect_id`
  is supplied.

## Value

A data frame (`fastRhockey_data`) with the following columns:

|                       |           |                                           |
|-----------------------|-----------|-------------------------------------------|
| col_name              | types     | description                               |
| id                    | integer   | Unique prospect record identifier.        |
| birth_city            | character | Prospect birth city.                      |
| birth_country3code    | character | Prospect birth country three-letter code. |
| birth_date            | character | Prospect date of birth.                   |
| birth_state_prov_code | character | Prospect birth state/province code.       |
| category_id           | integer   | Prospect category identifier.             |
| created_on            | character | Record creation timestamp.                |
| cs_player_id          | integer   | Central Scouting player identifier.       |
| draft_status_code     | character | Draft eligibility status code.            |
| ep_player_id          | integer   | EliteProspects player identifier.         |
| first_name            | character | Prospect first name.                      |
| headshot_id           | integer   | Headshot image identifier.                |
| height                | integer   | Prospect height (inches).                 |
| hometown              | character | Prospect hometown.                        |
| last_club_name        | character | Most recent club name.                    |
| last_league_abbr      | character | Most recent league abbreviation.          |
| last_name             | character | Prospect last name.                       |
| nationality_code      | character | Prospect nationality code.                |
| news_articles         | character | Associated news articles.                 |
| playerid              | integer   | Unique player identifier.                 |
| position_desc         | character | Player position description.              |
| profile               | character | Prospect profile text.                    |
| quotes                | character | Quotes about the prospect.                |
| scouting_report       | character | Scouting report text.                     |
| shoots_catches        | character | Shooting/catching handedness.             |
| stats_text            | character | Statistical summary text.                 |
| video                 | character | Associated video content.                 |
| weight                | integer   | Prospect weight (pounds).                 |

## Examples

``` r
# \donttest{
  try(nhl_records_draft_prospect())
#> ── NHL Records Draft Prospect ───────────────────────────── fastRhockey 1.0.0 ──
#> ℹ Data updated: 2026-05-29 18:27:32 UTC
#> # A tibble: 135,792 × 28
#>       id birth_city birth_country3code birth_date birth_state_prov_code
#>    <int> <chr>      <chr>              <chr>      <chr>                
#>  1     1 Markham    CAN                1990-02-07 ON                   
#>  2     4 Houston    USA                1990-02-01 TX                   
#>  3     5 Saskatoon  CAN                1989-11-02 SK                   
#>  4     6 King City  CAN                1990-01-18 ON                   
#>  5     9 Toronto    CAN                1990-02-18 ON                   
#>  6    10 Greenwich  USA                1989-10-20 CT                   
#>  7    12 Oberageri  CHE                1990-01-30 NA                   
#>  8    13 Dauphin    CAN                1990-04-24 MB                   
#>  9    18 Whiterock  CAN                1990-03-08 BC                   
#> 10    22 Courtice   CAN                1990-06-05 ON                   
#> # ℹ 135,782 more rows
#> # ℹ 23 more variables: category_id <int>, created_on <chr>, cs_player_id <int>,
#> #   draft_status_code <chr>, ep_player_id <int>, first_name <chr>,
#> #   headshot_id <int>, height <int>, hometown <chr>, last_club_name <chr>,
#> #   last_league_abbr <chr>, last_name <chr>, nationality_code <chr>,
#> #   news_articles <chr>, playerid <int>, position_desc <chr>, profile <chr>,
#> #   quotes <chr>, scouting_report <chr>, shoots_catches <chr>, …
# }
```
