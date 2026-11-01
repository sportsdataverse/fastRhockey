---
name: Bug Report
about: Report something that isn't working correctly
title: "[BUG] "
labels: bug
assignees: ''
---

## Function and arguments

<!-- Which fastRhockey function(s) are affected? Give the exact arguments (game id, season, team, league ...). -->

```r
pwhl_pbp(game_id = 27)
```

## Description

<!-- Clear, concise description of the bug -->

## Reproduction Steps

1. 
2. 
3. 

## Expected Behavior

<!-- What should happen -->

## Actual Behavior

<!-- What actually happens -- include the full error message or stack trace -->

```
# Paste error output here
```

## Minimal Reproducible Example

<!-- Ideally made with reprex::reprex() -->

```r
# Paste a minimal, self-contained example that reproduces the bug
library(fastRhockey)


```

## Environment

- **fastRhockey version:** <!-- e.g., 1.0.0 — run packageVersion("fastRhockey") -->
- **R version:** <!-- e.g., 4.4.1 — run R.version.string -->
- **OS:** <!-- e.g., macOS 14.5, Windows 11, Ubuntu 22.04 -->
- **Installed via:** <!-- CRAN / pak::pak("sportsdataverse/fastRhockey") -->

<details>
<summary>sessionInfo()</summary>

```r
# Paste the output of sessionInfo() here
```

</details>

## Additional Context

<!-- Related issues, workarounds, or links to relevant API docs. Say whether you are behind a proxy (getOption("fastRhockey.proxy")) or on a cloud / datacenter IP: some providers, such as khl.ru, block those. -->
