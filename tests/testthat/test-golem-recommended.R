library(golem)

test_that("app ui", {
  ui <- .app_ui()
  golem::expect_shinytag(ui)
})

test_that("app server", {
  server <- .app_server
  expect_is(server, "function")
})
