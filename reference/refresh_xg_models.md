# Force re-download of xG models

Clears the cached xG model files, downloads them again from the
sportsdataverse-data `nhl_xg_models` release and reloads them. The
models normally load on first use and a stale cached copy is replaced
automatically, so this is only needed to repair a cache by hand.

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
