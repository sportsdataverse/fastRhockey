## Summary

<!-- 2-3 sentences: what does this PR do at a high level? -->

## Type of Change

- [ ] Bug fix (non-breaking change that fixes an issue)
- [ ] New feature (non-breaking change that adds functionality)
- [ ] Breaking change (fix or feature that would cause existing functionality to change)
- [ ] Documentation update
- [ ] Deprecation (marking functions for future removal)
- [ ] CI/build (workflow, package config, or infrastructure changes)

## Changes Made

| File | Change | Why |
|------|--------|-----|
| | | |

## Rationale & Decision Log

<!-- WHY changes were made; alternatives considered; trade-offs -->

## Testing & Validation

- [ ] `devtools::check()` passes with no ERRORs or WARNINGs
- [ ] `devtools::test()` -- all existing tests pass
- [ ] New tests added for new/changed functions
- [ ] Tested on at least one platform (macOS / Windows / Linux)

### Validation Evidence

<!-- Screenshots, test output, or example results -->

## Documentation

- [ ] roxygen2 docs updated (`devtools::document()`)
- [ ] `NEWS.md` updated with user-facing changes
- [ ] Vignettes updated (if applicable)
- [ ] pkgdown site builds (`pkgdown::build_site()`)

## Checklist

- [ ] Branch follows naming convention: `<type>/<short-description>`
- [ ] Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/) (`<type>(<scope>): <description>`)
- [ ] No debug code, print statements, or TODO comments without context
- [ ] NAMESPACE regenerated if imports/exports changed
- [ ] DESCRIPTION version bumped (if releasing)

> **Reminder:** Run `devtools::check()` and `devtools::test()` before requesting review.
> Use Conventional Commits: `feat(nhl):`, `fix(pwhl):`, `docs(pkgdown):`, `test(nhl):`, etc.

## Rollback Plan

<!-- Specific steps to revert if needed -->
