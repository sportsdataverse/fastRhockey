# NA

## Summary

## Type of Change

Bug fix (non-breaking change that fixes an issue)

New feature (non-breaking change that adds functionality)

Breaking change (fix or feature that would cause existing functionality
to change)

Documentation update

Deprecation (marking functions for future removal)

CI/build (workflow, package config, or infrastructure changes)

## Changes Made

| File | Change | Why |
|------|--------|-----|
|      |        |     |

## Rationale & Decision Log

## Testing & Validation

`devtools::check()` passes with no ERRORs or WARNINGs

`devtools::test()` – all existing tests pass

New tests added for new/changed functions

Tested on at least one platform (macOS / Windows / Linux)

### Validation Evidence

## Documentation

roxygen2 docs updated (`devtools::document()`)

`NEWS.md` updated with user-facing changes

Vignettes updated (if applicable)

pkgdown site builds
([`pkgdown::build_site()`](https://pkgdown.r-lib.org/reference/build_site.html))

## Checklist

Branch follows naming convention: `<type>/<short-description>`

Commit messages follow [Conventional
Commits](https://www.conventionalcommits.org/)
(`<type>(<scope>): <description>`)

No debug code, print statements, or TODO comments without context

NAMESPACE regenerated if imports/exports changed

DESCRIPTION version bumped (if releasing)

> **Reminder:** Run `devtools::check()` and `devtools::test()` before
> requesting review. Use Conventional Commits: `feat(nhl):`,
> `fix(pwhl):`, `docs(pkgdown):`, `test(nhl):`, etc.

## Rollback Plan
