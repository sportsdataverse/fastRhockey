test_that("NHL - Get NHL Game Content", {
  skip_on_cran()
  x <- nhl_game_content(game_id=2021020182)
  
  cols <- c("type",
             "id",
             "date",
             "title",
             "blurb",
             "description",
             "duration",
             "authFlow",
             "mediaPlaybackId",
             "mediaState",
             "keywords",
             "image",
             "playbacks")
  
  expect_equal(colnames(x), cols)
  expect_s3_class(x, 'data.frame')
  
})
