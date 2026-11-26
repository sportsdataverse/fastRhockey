# **Get ESPN MCH / WCH News**

**Get ESPN MCH / WCH News**

## Usage

``` r
espn_mch_news(limit = 50, ...)

espn_wch_news(limit = 50, ...)
```

## Arguments

- limit:

  Maximum number of articles to return (default `50`).

- ...:

  Reserved for forward compatibility.

## Value

A `fastRhockey_data` tibble with one row per article:

|               |           |                                         |
|---------------|-----------|-----------------------------------------|
| col_name      | type      | description                             |
| article_id    | character | ESPN article identifier.                |
| now_id        | character | ESPN Now identifier.                    |
| content_key   | character | Content management key.                 |
| type          | character | Article type (e.g. "Story", "Media").   |
| headline      | character | Article headline.                       |
| description   | character | Article description/excerpt.            |
| last_modified | character | ISO 8601 last-modified datetime.        |
| published     | character | ISO 8601 published datetime.            |
| byline        | character | Author byline.                          |
| premium       | logical   | Whether the article is premium content. |
| image_href    | character | URL of the lead image.                  |
| web_href      | character | URL of the article on ESPN.com.         |

## Details

League slug `mens-college-hockey`; `season` is the END year (`2026` =
2025-26). Same ESPN helper as
[`espn_nhl_news()`](https://fastRhockey.sportsdataverse.org/reference/espn_nhl_news.md);
columns verified against a captured MCH body.

`espn_wch_news()` is the same wrapper for women's college hockey (league
slug `womens-college-hockey`): same arguments, same columns.

## See also

[family_espn_mch](https://fastRhockey.sportsdataverse.org/reference/family_espn_mch.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_mch_news(limit = 10))
#> ── MENS-COLLEGE-HOCKEY News data from ESPN.com ──────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-26 22:24:09 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50557743   1-50557743 50557743-1-29… Media Boston … Boston Col… 2026-11-22T0…
#>  2 50556977   1-50556977 50556977-1-29… Media Michiga… Michigan W… 2026-11-22T0…
#>  3 50556929   1-50556929 50556929-1-29… Media Merrima… Merrimack … 2026-11-22T0…
#>  4 50556334   1-50556334 50556334-1-29… Media Massach… Massachuse… 2026-11-22T0…
#>  5 50545990   1-50545990 50545990-1-29… Media Michiga… Michigan S… 2026-11-21T0…
#>  6 50545899   1-50545899 50545899-1-29… Media Boston … Boston Col… 2026-11-21T0…
#>  7 50536655   1-50536655 50536655-1-29… Media New Ham… New Hampsh… 2026-11-20T0…
#>  8 50535991   1-50535991 50535991-1-29… Media Provide… Providence… 2026-11-20T0…
#>  9 50526900   1-50526900 50526900-1-29… Media Quinnip… Quinnipiac… 2026-11-19T0…
#> 10 50486049   1-50486049 50486049-1-29… Media Massach… Massachuse… 2026-11-15T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
# \donttest{
  try(espn_wch_news(limit = 10))
#> ── WOMENS-COLLEGE-HOCKEY News data from ESPN.com ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-26 22:24:09 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50554582   1-50554582 50554582-1-29… Media Quinnip… Quinnipiac… 2026-11-21T2…
#>  2 50553826   1-50553826 50553826-1-29… Media Ohio St… Ohio St. B… 2026-11-21T2…
#>  3 50545822   1-50545822 50545822-1-29… Media Maine B… Maine Blac… 2026-11-21T0…
#>  4 50545329   1-50545329 50545329-1-29… Media Uconn H… Uconn Husk… 2026-11-21T0…
#>  5 50545243   1-50545243 50545243-1-29… Media Quinnip… Quinnipiac… 2026-11-21T0…
#>  6 50545190   1-50545190 50545190-1-29… Media Princet… Princeton … 2026-11-21T0…
#>  7 50543742   1-50543742 50543742-1-29… Media Colgate… Colgate Ra… 2026-11-20T2…
#>  8 50543595   1-50543595 50543595-1-29… Media Boston … Boston Col… 2026-11-20T2…
#>  9 50542358   1-50542358 50542358-1-29… Media Boston … Boston U. … 2026-11-20T1…
#> 10 50484363   1-50484363 50484363-1-29… Media Boston … Boston Col… 2026-11-14T2…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
