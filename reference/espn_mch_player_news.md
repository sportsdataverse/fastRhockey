# **Get ESPN MCH / WCH Player News**

**Get ESPN MCH / WCH Player News**

## Usage

``` r
espn_mch_player_news(athlete_id, limit = 50, ...)

espn_wch_player_news(athlete_id, limit = 50, ...)
```

## Arguments

- athlete_id:

  ESPN athlete identifier (character or numeric, e.g. `"5136656"`).
  Obtain via
  [`espn_mch_team_roster()`](https://fastRhockey.sportsdataverse.org/reference/espn_mch_team_roster.md).

- limit:

  Maximum number of articles to return (default `50`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per news article, or an empty
[`data.frame()`](https://rdrr.io/r/base/data.frame.html) when no
articles are available:

|                  |           |                                               |
|------------------|-----------|-----------------------------------------------|
| col_name         | types     | description                                   |
| athlete_id       | character | ESPN athlete identifier (echoed from arg).    |
| article_id       | character | Article identifier.                           |
| type             | character | Article type (e.g. "HeadlineNews").           |
| headline         | character | Article headline.                             |
| description      | character | Article description / lede.                   |
| published        | character | Publication datetime (ISO 8601).              |
| last_modified    | character | Last-modified datetime (ISO 8601).            |
| premium          | logical   | Whether article is premium/paywalled.         |
| source           | character | News source.                                  |
| byline           | character | Author byline.                                |
| article_url      | character | Web URL for the full article.                 |
| image_url        | character | Lead image URL.                               |
| category_league  | character | League category label from article metadata.  |
| category_team    | character | Team category label from article metadata.    |
| category_athlete | character | Athlete category label from article metadata. |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_player_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_player_news.md)
(columns inherited); a collection may be empty.

`espn_wch_player_news()` is the same wrapper for women's college hockey
(league slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_player_news(athlete_id = "5136656", limit = 10))
#> ! No news articles found for athlete 5136656.
#> data frame with 0 columns and 0 rows
# }
# \donttest{
  try(espn_wch_player_news(athlete_id = "5263060", limit = 10))
#> ! No news articles found for athlete 5263060.
#> data frame with 0 columns and 0 rows
# }
```
