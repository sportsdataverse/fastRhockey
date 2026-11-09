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
#> ℹ Data updated: 2026-11-09 17:18:29 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50402609   1-50402609 50402609-1-6-1 Story NHL dra… Find out m… 2026-11-09T1…
#>  2 50416102   1-50416102 50416102-1-29… Media Yale El… Yale Elis,… 2026-11-08T0…
#>  3 50415741   1-50415741 50415741-1-29… Media Massach… Massachuse… 2026-11-08T0…
#>  4 50414401   1-50414401 50414401-1-29… Media Provide… Providence… 2026-11-08T0…
#>  5 50404291   1-50404291 50404291-1-29… Media Vermont… Vermont Ca… 2026-11-07T0…
#>  6 50404241   1-50404241 50404241-1-29… Media Quinnip… Quinnipiac… 2026-11-07T0…
#>  7 50404182   1-50404182 50404182-1-29… Media Massach… Massachuse… 2026-11-07T0…
#>  8 50395456   1-50395456 50395456-1-29… Media Uconn H… Uconn Husk… 2026-11-06T0…
#>  9 50395317   1-50395317 50395317-1-29… Media Vermont… Vermont Ca… 2026-11-06T0…
#> 10 50337994   1-50337994 50337994-1-29… Media Provide… Providence… 2026-10-31T1…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
# \donttest{
  try(espn_wch_news(limit = 10))
#> ── WOMENS-COLLEGE-HOCKEY News data from ESPN.com ────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 17:18:29 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50414981   1-50414981 50414981-1-29… Media Cornell… Cornell Bi… 2026-11-08T0…
#>  2 50414280   1-50414280 50414280-1-29… Media Maine B… Maine Blac… 2026-11-08T0…
#>  3 50413107   1-50413107 50413107-1-29… Media Harvard… Harvard Cr… 2026-11-07T2…
#>  4 50412798   1-50412798 50412798-1-29… Media Northea… Northeaste… 2026-11-07T2…
#>  5 50412657   1-50412657 50412657-1-29… Media Colgate… Colgate Ra… 2026-11-07T2…
#>  6 50403728   1-50403728 50403728-1-29… Media Maine B… Maine Blac… 2026-11-07T0…
#>  7 50403619   1-50403619 50403619-1-29… Media Cornell… Cornell Bi… 2026-11-07T0…
#>  8 50403613   1-50403613 50403613-1-29… Media Boston … Boston Col… 2026-11-07T0…
#>  9 50403587   1-50403587 50403587-1-29… Media Colgate… Colgate Ra… 2026-11-07T0…
#> 10 50392223   1-50392223 50392223-1-29… Media Uconn H… Uconn Husk… 2026-11-05T2…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
