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
#> ℹ Data updated: 2026-11-26 22:24:47 UTC
#> # A tibble: 10 × 12
#>    article_id now_id     content_key    type  headline description last_modified
#>    <chr>      <chr>      <chr>          <chr> <chr>    <chr>       <chr>        
#>  1 50605811   1-50605811 50605811-1-5-1 Head… Devils … Devils def… 2026-11-26T2…
#>  2 50576889   1-50576889 50576889-1-6-1 Story Friday … A Seton Ha… 2026-11-26T1…
#>  3 50602828   1-50602828 50602828-1-22… Prev… Maple L… Toronto Ma… 2026-11-26T0…
#>  4 50602827   1-50602827 50602827-1-22… Prev… Golden … Vegas Gold… 2026-11-26T0…
#>  5 50602826   1-50602826 50602826-1-22… Prev… Oilers … Seattle Kr… 2026-11-26T0…
#>  6 50602825   1-50602825 50602825-1-22… Prev… Devils … Calgary Fl… 2026-11-26T0…
#>  7 50602824   1-50602824 50602824-1-22… Prev… Ducks p… Los Angele… 2026-11-26T0…
#>  8 50602823   1-50602823 50602823-1-22… Prev… Predato… Utah Mammo… 2026-11-26T0…
#>  9 50602822   1-50602822 50602822-1-22… Prev… Chicago… New York R… 2026-11-26T0…
#> 10 50602821   1-50602821 50602821-1-22… Prev… Red Win… Detroit Re… 2026-11-26T0…
#> # ℹ 5 more variables: published <chr>, byline <chr>, premium <lgl>,
#> #   image_href <chr>, web_href <chr>
# }
```
