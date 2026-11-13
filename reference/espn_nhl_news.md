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
#> ℹ Data updated: 2026-11-13 21:27:05 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 49299647   1-49299647 49299647-1-6-1 Story Fantasy… Pyotr Koch… 2026-11-13T1…
#>  2 50472487   1-50472487 50472487-1-5-1 Head… Finland… The NHL wi… 2026-11-13T1…
#>  3 50471309   1-50471309 50471309-1-29… Media Which t… Which team… 2026-11-13T1…
#>  4 50464480   1-50464480 50464480-1-6-1 Story NHL Pow… Which team… 2026-11-13T1…
#>  5 50463496   1-50463496 50463496-1-6-1 Story NHL Ton… Find game … 2026-11-13T1…
#>  6 50469928   1-50469928 50469928-1-22… Prev… Florida… Florida Pa… 2026-11-13T0…
#>  7 50469927   1-50469927 50469927-1-22… Prev… Winnipe… Winnipeg J… 2026-11-13T0…
#>  8 50469926   1-50469926 50469926-1-22… Prev… Red Win… Detroit Re… 2026-11-13T0…
#>  9 50469925   1-50469925 50469925-1-22… Prev… Columbu… Philadelph… 2026-11-13T0…
#> 10 50469924   1-50469924 50469924-1-22… Prev… Anaheim… New York I… 2026-11-13T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
