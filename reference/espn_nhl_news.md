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
#> ℹ Data updated: 2026-11-24 19:32:12 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50583955   1-50583955 50583955-1-5-1 Head… Lightni… Lightning … 2026-11-24T1…
#>  2 50575154   1-50575154 50575154-1-6-1 Story NHL Ton… Find game … 2026-11-24T1…
#>  3 50576360   1-50576360 50576360-1-6-1 Story A myste… As teams s… 2026-11-24T1…
#>  4 48431082   1-48431082 48431082-1-6-1 Story Which N… Scotty Bow… 2026-11-24T0…
#>  5 50580314   1-50580314 50580314-1-21… Recap Joel Er… — Kirill K… 2026-11-24T0…
#>  6 50580262   1-50580262 50580262-1-29… Media Minneso… Minnesota … 2026-11-24T0…
#>  7 50580253   1-50580253 50580253-1-21… Recap Kaapo K… — Kaapo Ka… 2026-11-24T0…
#>  8 50580190   1-50580190 50580190-1-29… Media Anaheim… Anaheim Du… 2026-11-24T0…
#>  9 50579902   1-50579902 50579902-1-5-1 Head… Gavin M… Top pick G… 2026-11-24T0…
#> 10 50579428   1-50579428 50579428-1-21… Recap Ullmark… — Linus Ul… 2026-11-24T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
