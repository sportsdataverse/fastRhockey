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
if (FALSE) { # interactive()
# \donttest{
  try(nhl_records_draft_prospect())
# }
}
```
