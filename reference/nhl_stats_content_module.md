# **NHL Stats API — Content Module**

Returns an NHL.com CMS content module from the Stats REST API
(`https://api.nhle.com/stats/rest/{lang}/content/module/{template_key}`).

## Usage

``` r
nhl_stats_content_module(template_key, lang = "en")
```

## Arguments

- template_key:

  Character (required). The content module template key (NHL.com
  internal identifier).

- lang:

  Character language code. Default `"en"`.

## Value

A `fastRhockey_data` tibble of content module rows, or `NULL` on
failure.

## Examples

``` r
# \donttest{
  try(nhl_stats_content_module(template_key = "example"))
#> 2026-04-13 17:05:58.220846: No content module data for 'example'
#> NULL
# }
```
