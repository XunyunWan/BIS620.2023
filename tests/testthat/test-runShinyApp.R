test_that("shiny_app works", {
  result <- runShinyApp()
  expect_true(class(result) == "shiny.appobj")
})
