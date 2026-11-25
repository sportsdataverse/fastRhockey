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
#> ℹ Data updated: 2026-11-25 04:39:54 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50589650   1-50589650 50589650-1-21… Recap Connell… "— Trevor … 2026-11-25T0…
#>  2 50589542   1-50589542 50589542-1-29… Media Vegas G… "Vegas Gol… 2026-11-25T0…
#>  3 50586281   1-50586281 50586281-1-5-1 Head… Joel Ho… "Jordan Bi… 2026-11-24T2…
#>  4 50586215   1-50586215 50586215-1-5-1 Head… Remi Po… "Remi Poir… 2026-11-24T2…
#>  5 50583955   1-50583955 50583955-1-5-1 Head… Lightni… "Lightning… 2026-11-24T1…
#>  6 50575154   1-50575154 50575154-1-6-1 Story NHL Ton… "Find game… 2026-11-24T1…
#>  7 50576360   1-50576360 50576360-1-6-1 Story A myste… "As teams … 2026-11-24T1…
#>  8 48431082   1-48431082 48431082-1-6-1 Story Which N… "Scotty Bo… 2026-11-24T0…
#>  9 50580314   1-50580314 50580314-1-21… Recap Joel Er… "— Kirill … 2026-11-24T0…
#> 10 50580262   1-50580262 50580262-1-29… Media Minneso… "Minnesota… 2026-11-24T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
