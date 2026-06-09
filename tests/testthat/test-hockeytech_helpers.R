test_that(".hockeytech_url builds a feed URL with key + client_code", {
  u <- fastRhockey:::.hockeytech_url("pwhl", feed = "modulekit", view = "seasons", params = list(site_id = 0))
  expect_true(grepl("^https://lscluster.hockeytech.com/feed/index.php\\?", u))
  expect_true(grepl("feed=modulekit", u) && grepl("view=seasons", u))
  expect_true(grepl("key=446521baf8c38984", u) && grepl("client_code=pwhl", u))
})

test_that("gc feed uses tab not view", {
  u <- fastRhockey:::.hockeytech_url("pwhl", feed = "gc", view = "gamesummary", params = list(game_id = 42))
  expect_true(grepl("tab=gamesummary", u))
  expect_false(grepl("view=", u))
})

test_that("pbp view uses the override key", {
  u <- fastRhockey:::.hockeytech_url("pwhl", feed = "statviewfeed", view = "gameCenterPlayByPlay",
                                     params = list(game_id = 42))
  expect_true(grepl("key=694cfeed58c932ee", u))
})
