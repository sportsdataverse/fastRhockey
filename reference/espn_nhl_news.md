# **Get ESPN NHL News**

**Get ESPN NHL News**

## Usage

``` r
espn_nhl_news(limit = 50, ...)
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
| col_name      | types     | description                             |
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

## See also

[family_espn_nhl](https://fastRhockey.sportsdataverse.org/reference/family_espn_nhl.md)
lists every function in this family.

## Author

Saiem Gilani

## Examples

``` r
# \donttest{
  try(espn_nhl_news(limit = 10))
#> ── NHL News data from ESPN.com ──────────────────────────── fastRhockey 1.1.0 ──
#> ℹ Data updated: 2026-11-09 11:49:55 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50430877   1-50430877 50430877-1-22… Prev… Blues b… Detroit Re… 2026-11-09T0…
#>  2 50430876   1-50430876 50430876-1-22… Prev… Canucks… Philadelph… 2026-11-09T0…
#>  3 50430875   1-50430875 50430875-1-22… Prev… Devils … New Jersey… 2026-11-09T0…
#>  4 50430874   1-50430874 50430874-1-22… Prev… Maple L… Colorado A… 2026-11-09T0…
#>  5 50430873   1-50430873 50430873-1-22… Prev… Predato… Nashville … 2026-11-09T0…
#>  6 50430872   1-50430872 50430872-1-22… Prev… Senator… Washington… 2026-11-09T0…
#>  7 50430871   1-50430871 50430871-1-22… Prev… Canadie… Minnesota … 2026-11-09T0…
#>  8 50430870   1-50430870 50430870-1-22… Prev… Blue Ja… Florida Pa… 2026-11-09T0…
#>  9 41679163   1-41679163 41679163-1-6-1 Story Which p… Wayne Gret… 2026-11-09T0…
#> 10 48431082   1-48431082 48431082-1-6-1 Story Which N… Scotty Bow… 2026-11-09T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
