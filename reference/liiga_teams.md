# **Liiga teams**

Team directory: every current and past team with logos, venue capacity
and club info.

## Usage

``` r
liiga_teams()
```

## Value

A `fastRhockey_data` tibble (14 columns; zero rows when a valid payload
has none).

|  |  |  |
|----|----|----|
| col_name | type | description |
| `team_id` | integer | Team id, the number of Liiga's `<num>:<slug>` team key (sent as that string, a bare integer or a digit string depending on the route). |
| `team_slug` | character | Team slug, the part of Liiga's `<num>:<slug>` team key after the colon (e.g. hpk). |
| `name` | character | Club name as Liiga lists it, e.g. HIFK or K-Espoo. |
| `contact_info` | character | Club contact details as HTML, in Finnish (company, address, phone). |
| `externals` | character | JSON list of the club's external links (url, label), e.g. its ticket shops. |
| `country_code` | character | Club's country, 3-letter code (e.g. FIN). |
| `country_name` | character | Club's country name, e.g. Finland. |
| `current_venue_capacity` | integer | Capacity of the team's current home arena (spectators). |
| `general_info` | character | Club history and facts as an HTML table, in Finnish (founding year, titles, ...). |
| `url` | character | Club website URL. |
| `locality` | character | Club's home locality; null in every capture (unverified). |
| `logo` | character | Team logo image URL (old.liiga.fi media). |
| `short_name` | character | Team abbreviation. |
| `teamtournamentstats` | character | Team totals per season and tournament, snake_case, percentages and averages sent as STRINGS. |

## Details

Endpoint: `GET https://liiga.fi/api/v2/teams/info`

Seasons are END years (2026 = 2025-26), the same key as
sportsdataverse-py. Requests to liiga.fi are spaced at least one second
apart. A request Liiga answers with nothing (an unknown game, a
tournament not played that season, a shot map before 2016) raises
`fastRhockey_no_data`; a failed fetch or an error-shaped answer raises
`fastRhockey_asset_fetch`; a bad argument raises
`fastRhockey_bad_request`.

## See also

[family_liiga](https://fastRhockey.sportsdataverse.org/reference/family_liiga.md)
lists every function in this family.

## Examples

``` r
if (FALSE) { # interactive()
# \donttest{
  try(liiga_teams())
# }
}
```
