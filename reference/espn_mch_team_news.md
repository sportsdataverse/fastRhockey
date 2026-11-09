# **Get ESPN MCH / WCH Team News**

**Get ESPN MCH / WCH Team News**

## Usage

``` r
espn_mch_team_news(team_id, limit = 50, ...)

espn_wch_team_news(team_id, limit = 50, ...)
```

## Arguments

- team_id:

  ESPN team identifier (character or numeric, e.g. `"104"` for Chicago
  Blackhawks).

- limit:

  Maximum number of articles to return. Default `50`.

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per news article when data is
available. The `/news` endpoint currently returns an empty response for
MCH; an empty tibble is returned with a cli warning. Columns when
populated include `team_id`, `id`, `headline`, `description`, `type`,
`published`, `last_modified`, `premium`, and `url`.

Columns when populated:

|               |           |                                                  |
|---------------|-----------|--------------------------------------------------|
| col_name      | type      | description                                      |
| team_id       | character | ESPN team identifier (echoed from the argument). |
| id            | character | Article identifier.                              |
| headline      | character | Article headline.                                |
| description   | character | Article description.                             |
| type          | character | Article type.                                    |
| published     | character | Publication timestamp.                           |
| last_modified | character | Last-modified timestamp.                         |
| premium       | logical   | Whether the article is premium content.          |
| url           | character | Web URL of the article.                          |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_team_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_team_news.md)
(columns inherited); a collection may be empty.

`espn_wch_team_news()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_team_news(team_id = "104", limit = 10))
#> ! ESPN MENS-COLLEGE-HOCKEY team news returned no data for team_id=104. This endpoint may not be populated for MENS-COLLEGE-HOCKEY.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_team_news(team_id = "2137", limit = 10))
#> ! ESPN WOMENS-COLLEGE-HOCKEY team news returned no data for team_id=2137. This endpoint may not be populated for WOMENS-COLLEGE-HOCKEY.
#> data frame with 0 columns and 0 rows
# }
```
