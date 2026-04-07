# Load xG models from cache or download from GitHub

Models are cached in the user's R cache directory
([`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html)) so they
are only downloaded once.

## Usage

``` r
.load_xg_models(pkgname)
```
