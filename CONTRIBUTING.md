# Contributing to fastRhockey

Thank you for your interest in contributing to fastRhockey! This document
provides guidelines for contributing to the project.

## Getting Started

### Prerequisites

- R >= 4.1.0
- [devtools](https://devtools.r-lib.org/) package
- Git

### Development Setup

```r
# Clone the repository
# git clone https://github.com/sportsdataverse/fastRhockey.git

# Install all dependencies (including Suggests)
devtools::install_deps(dependencies = TRUE)

# Build documentation
devtools::document()

# Verify everything works
devtools::check()
```

## Development Workflow

1. **Create a branch** from `main` using the naming convention:
   `<type>/<short-description>` (e.g., `feat/add-pwhl-draft`,
   `fix/schedule-null-handling`)

2. **Make your changes** following the coding conventions below

3. **Write tests** for any new or changed functions

4. **Run checks** before submitting:
   ```r
   devtools::document()  # Regenerate docs
   devtools::test()      # Run tests
   devtools::check()     # Full R CMD check
   ```

5. **Submit a pull request** using the PR template

## Coding Conventions

### File Organization

- One exported function per file
- Filename matches the function name (e.g., `nhl_standings.R` contains
  `nhl_standings()`)
- Internal helper functions prefixed with `.` (e.g., `.parse_response()`)

### Style Guide

- Use `%>%` (magrittr pipe) for chaining
- Use `janitor::clean_names()` on all API responses
- Make HTTP requests through the package's **httr2** helpers in `R/utils.R`,
  never `httr` (it is not a dependency): `.retry_request(url, params = ...)`
  performs a GET with `httr2::req_retry()` and the proxy resolution described
  under [Proxy support](#proxy-support); `.resp_text()` / `.resp_json()` read
  the body
- Use `glue::glue()` for URL string interpolation
- Maximum line length: no strict limit, but keep readable

### Error Handling

The package has three failure contracts. Follow the one of the family you
are working in.

**NHL and PWHL wrappers** report the error with a message and return `NULL`
(a few return an empty data frame):

```r
tryCatch(
    expr = {
        res <- .retry_request(url)
        check_status(res)
        raw <- jsonlite::fromJSON(.resp_text(res), flatten = TRUE)
        # ... parse and return
    },
    error = function(e) {
        message(glue::glue("{Sys.time()}: Error in func_name: {e$message}"))
        return(NULL)
    }
)
```

**ESPN (NHL and men's/women's college), Fox Sports and AHL / OHL / WHL /
QMJHL wrappers** report the error through `.report_api_error()` (or
`cli::cli_alert_danger()`) and return an empty data frame.

**European league wrappers** (`del_*`, `khl_*`, `shl_*`, `liiga_*`, ...)
build the request with `.build_request()` and perform it through
`.polite_perform()` or `.euro_get()` (`R/euro_hockey_runtime.R`). These
space requests to one host at least a second apart and retry only HTTP 429,
500, 502, 503 and 504 (plus connection failures), honouring `Retry-After`
up to 120 s. On failure they raise a classed condition rather than returning
`NULL`, all inheriting `fastRhockey_error`:

- `fastRhockey_no_data`: HTTP 404, or the provider has nothing for the request.
- `fastRhockey_bad_request`: HTTP 400 or 422.
- `fastRhockey_asset_fetch`: any other non-2xx status (including a 429 or 5xx
  still failing after the retries), a connection failure, a block page, or an
  empty or wrong body.

### S3 Class

All data-returning functions must wrap output with `make_fastRhockey_data()`:

```r
result <- make_fastRhockey_data(df, type = "Description of data", timestamp = Sys.time())
```

### Documentation

- All exported functions require roxygen2 documentation
- Include `@examples` with `\donttest{ try(...) }` wrapper
- Run `devtools::document()` to regenerate `man/` pages
- Edit `README.Rmd`, never `README.md`, and knit it with
  `devtools::build_readme()`. Its function counts read the *installed*
  package's exports, and `build_readme()` installs the working copy first;
  a plain `rmarkdown::render()` counts whatever version is installed (CRAN
  1.0.0 gives 423 instead of 880)

## General Naming Conventions

### Exported Functions

Use the `league_entity_action()` pattern:

- `nhl_teams_roster()` -- NHL team roster
- `pwhl_schedule()` -- PWHL schedule
- `khl_standings()` -- KHL standings
- `espn_mch_scoreboard()` -- ESPN men's college hockey scoreboard
- `nhl_player_stats()` -- NHL player statistics

### Internal Helpers

Prefix with a leading dot, use `snake_case`:

- `.parse_game_rosters()`
- `.build_pbp()`
- `.extract_scores()`

### Test Files

Name test files `test-function_name.R` to match the function under test:

- `test-nhl_standings.R`
- `test-pwhl_schedule.R`

### Datasets

Use `snake_case` for `.rda` files in `data/`:

- `phf_teams.rda`

### Parameters

Use `snake_case` for all function parameters:

- `game_id`, `team_abbr`, `season`, `player_id`

### Season Format Conventions

Different APIs use different season formats:

| Context | Format | Example |
|---------|--------|---------|
| NHL Web API / Stats API parameter | Concatenated years string | `"20242025"` |
| `most_recent_nhl_season()` return value | Numeric end year | `2025` |
| `most_recent_nhl_season_api_param()` return value | Concatenated string | `"20242025"` |
| PWHL / HockeyTech / ESPN / European season parameter | Numeric end year | `2026` (= 2025-26) |

## Testing

- **Framework:** testthat edition 3
- **Location:** `tests/testthat/`
- **File naming:** `test-<function_name>.R`

### Test Template

```r
test_that("LEAGUE - Descriptive test name", {
    skip_on_cran()
    skip_nhl_test()  # or skip_pwhl_test(), skip_espn_test(), skip_euro_test()
    x <- function_name(param = value)

    expect_s3_class(x, "data.frame")
    expect_s3_class(x, "fastRhockey_data")
    expect_true(nrow(x) > 0)

    expected_cols <- c("col1", "col2")
    for (col in expected_cols) {
        expect_true(col %in% names(x), info = paste("Missing column:", col))
    }
})
```

### Test Conventions

- the family's `skip_<family>_test()` on all tests hitting live APIs (it
  already skips on CRAN and CI); offline tests (mocks, fixtures, captured
  bodies) need no skip
- Check both `data.frame` and `fastRhockey_data` S3 classes
- Validate `nrow(x) > 0`
- Check expected column names
- Use specific game IDs / dates for reproducibility
- xG tests: add `skip_if_not_installed("xgboost")`

### Testing Environment Variables

Most tests in this package make **live API calls**. The rule: live tests
always skip on CRAN and CI; run them locally. Offline tests (mocks, fixtures,
captured bodies) run everywhere. To run the live tests on your machine:

- **`NOT_CRAN=true`**: Set this environment variable so that `skip_on_cran()`
  does not skip tests. You can set it in your `.Renviron` file or inline:
  ```r
  Sys.setenv(NOT_CRAN = "true")
  devtools::test()
  ```

- **Family test toggles** (via `tests/testthat/helper-skip.R`). Every live
  helper goes through `skip_live_test()`: it calls `skip_on_cran()` and
  `skip_on_ci()`, then runs the test only when its toggle is `1` or `true`
  (any case). An unset toggle skips, so every live family is off until you
  turn it on:

  | Variable | Controls |
  |----------|----------|
  | `RUN_NHL_TESTS` | NHL API tests (`skip_nhl_test()`) |
  | `RUN_PWHL_TESTS` | PWHL API tests (`skip_pwhl_test()`) |
  | `RUN_ESPN_TESTS` | ESPN NHL tests (`skip_espn_test()`) |
  | `RUN_EURO_TESTS` | European league and ESPN college hockey (`espn_mch_*` / `espn_wch_*`) live tests (`skip_euro_test()`) |
  | `RUN_PHF_TESTS` | Defunct PHF tests (`skip_phf_test()`) |
  | `RUN_SLOW_NHL_TESTS` | The two slow season-wide NHL aggregator tests (`skip_slow_nhl_test()`, on top of `RUN_NHL_TESTS`) |

  Set them before running tests:
  ```r
  Sys.setenv(NOT_CRAN = "true", RUN_NHL_TESTS = "1", RUN_EURO_TESTS = "1")
  devtools::test()
  ```

  CI (`R-CMD-check.yaml`, `test-coverage.yaml`) passes the toggles through
  from repository variables, but `skip_on_ci()` still skips every live test
  there, so CI runs the offline tests only.

- **`GITHUB_PAT`**: Some loader functions access data from GitHub repositories
  under the `sportsdataverse` org. Set a GitHub personal access token to avoid
  rate limits:
  ```r
  # In ~/.Renviron
  GITHUB_PAT=ghp_your_token_here
  ```

- **Reproducible test data**: Use specific, known game IDs, dates, and seasons
  in tests (e.g., `game_id = 2023020001`, `season = 2024`) rather than
  `most_recent_*_season()` helpers, so test expectations remain stable across
  seasons.

## Conventional Commits

This project uses [Conventional Commits](https://www.conventionalcommits.org/)
for all commit messages. The format is:

```
<type>(<scope>): <description>
```

### Types

| Type | When to Use |
|------|-------------|
| `feat` | A new feature or function |
| `fix` | A bug fix |
| `docs` | Documentation-only changes |
| `style` | Formatting, whitespace, semicolons (no logic change) |
| `refactor` | Code change that neither fixes a bug nor adds a feature |
| `test` | Adding or updating tests |
| `build` | Changes to build system or dependencies (DESCRIPTION, NAMESPACE) |
| `ci` | Changes to CI configuration (GitHub Actions workflows) |
| `chore` | Maintenance tasks (version bumps, config updates) |

### Scopes

| Scope | Covers |
|-------|--------|
| `nhl` | NHL functions and helpers |
| `phf` | PHF functions (defunct league) |
| `pwhl` | PWHL functions |
| `hockeytech` | AHL / OHL / WHL / QMJHL and the shared HockeyTech helpers |
| `espn` | ESPN NHL and college hockey functions |
| `fox` | Fox Sports NHL functions |
| `euro` | European league functions (or the league prefix, e.g. `khl`, `shl`) |
| `xg` | Expected goals model pipeline |
| `pkgdown` | pkgdown site and vignettes |
| `ci` | GitHub Actions and CI/CD |
| `loader` | Data loader functions |

### Examples

```
feat(pwhl): add pwhl_draft() function
fix(nhl): handle null venue in schedule response
test(pwhl): add tests for pwhl_standings
docs(pkgdown): update reference index grouping
build(nhl): add lifecycle to Imports
ci: update R-CMD-check matrix to R 4.4
refactor(nhl): extract common parsing into .parse_schedule()
```

## Deprecation Process

When a function needs to be deprecated (e.g., an API is retired or a function
is superseded by a better alternative), use the
[lifecycle](https://lifecycle.r-lib.org/) package:

### Step 1: Add the deprecation warning

```r
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This function is deprecated. Use [new_function()] instead.
my_old_function <- function(param) {
    lifecycle::deprecate_warn(
        when = "1.0.0",
        what = "my_old_function()",
        with = "my_new_function()"
    )
    # Delegate to the new function
    my_new_function(param)
}
```

### Step 2: Update the roxygen2 tags

- Add the `lifecycle::badge("deprecated")` to `@description`
- Add `@seealso` pointing to the replacement function
- Keep the function exported so existing code does not break immediately

### Step 3: Update package infrastructure

- Ensure `lifecycle` is in the `Imports` field of `DESCRIPTION`
- Call `usethis::use_lifecycle()` if lifecycle infrastructure is not yet set up
  (adds the badge images and roxygen2 configuration)

### Step 4: Document in NEWS.md

Add a **Deprecations** section in `NEWS.md` listing every deprecated function
and its replacement.

## Proxy support

All HTTP requests flow through an internal `httr2` helper (`.retry_request()`)
that resolves a proxy in this order:

1. an explicit `proxy =` argument (only on wrappers that thread `...`);
2. `getOption("fastRhockey.proxy")` — set once per session with
   `options(fastRhockey.proxy = "http://host:port")`;
3. the `http_proxy` / `https_proxy` / `no_proxy` environment variables.

The proxy value may be a URL string (`"http://host:port"`) or a named list
spread into `httr2::req_proxy()` for authenticated proxies
(`list(url=, port=, username=, password=, auth=)`). The session-`options()`
form is recommended when one proxy covers many calls.

## Reporting Issues

- Use the [bug report template](.github/ISSUE_TEMPLATE/bug_report.md) for bugs
- Use the [feature request template](.github/ISSUE_TEMPLATE/feature_request.md)
  for enhancements
- Include a minimal reproducible example when possible

## Code of Conduct

Be respectful and constructive in all interactions. This is a community
project maintained by volunteers.
