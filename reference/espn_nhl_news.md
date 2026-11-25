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
#> ℹ Data updated: 2026-11-25 20:18:06 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50596079   1-50596079 50596079-1-5-1 Head… Cale Ma… "The Avala… 2026-11-25T2…
#>  2 50594065   1-50594065 50594065-1-5-1 Head… Aleksan… "Aleksande… 2026-11-25T1…
#>  3 50585269   1-50585269 50585269-1-6-1 Story Why Ale… "The 2020 … 2026-11-25T1…
#>  4 50587368   1-50587368 50587368-1-6-1 Story NHL Tha… "Teams in … 2026-11-25T1…
#>  5 50502384   1-50502384 50502384-1-6-1 Story The Con… "Follow al… 2026-11-25T1…
#>  6 50584038   1-50584038 50584038-1-6-1 Story NHL Ton… "Find game… 2026-11-25T1…
#>  7 50589650   1-50589650 50589650-1-21… Recap Connell… "— Trevor … 2026-11-25T0…
#>  8 50589542   1-50589542 50589542-1-29… Media Vegas G… "Vegas Gol… 2026-11-25T0…
#>  9 50586281   1-50586281 50586281-1-5-1 Head… Joel Ho… "Jordan Bi… 2026-11-24T2…
#> 10 50586215   1-50586215 50586215-1-5-1 Head… Remi Po… "Remi Poir… 2026-11-24T2…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
