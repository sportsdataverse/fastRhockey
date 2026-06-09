test_that("hockeytech league registry has the five HockeyTech leagues", {
  leagues <- fastRhockey:::.hockeytech_leagues()
  expect_setequal(names(leagues), c("pwhl", "ahl", "ohl", "whl", "qmjhl"))
  expect_equal(leagues$pwhl$client_code, "pwhl")
  expect_equal(leagues$pwhl$league_id, 1)
  expect_equal(leagues$qmjhl$client_code, "lhjmq")
  expect_true(grepl("cluster.leaguestat.com", leagues$qmjhl$base_url))
})

test_that("env var overrides the api key", {
  withr::with_envvar(c(SDV_PWHL_API_KEY = "override123"), {
    expect_equal(fastRhockey:::.hockeytech_resolve_key("pwhl"), "override123")
  })
  withr::with_envvar(c(SDV_PWHL_API_KEY = NA), {
    expect_equal(fastRhockey:::.hockeytech_resolve_key("pwhl"), "446521baf8c38984")
  })
})

test_that("pbp view uses the override key", {
  expect_equal(fastRhockey:::.hockeytech_resolve_key("pwhl", view = "gameCenterPlayByPlay"),
               "694cfeed58c932ee")
})
