# Force re-download of xG models

Clears the cached xG model files and re-downloads them from the
fastRhockey-nhl-data repository. Use this after models have been
retrained and pushed to GitHub.

## Usage

``` r
refresh_xg_models()
```

## Value

Invisible NULL. Models are reloaded into the package namespace.

## Examples

``` r
if (FALSE) { # \dontrun{
  refresh_xg_models()
} # }
```
