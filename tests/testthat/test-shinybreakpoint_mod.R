test_that("js code in shinybreakpointUI is correct", {
  val <- shinybreakpointUI("test")
  expect_equal(
  val$children[[1]], tags$script(HTML(
  '  document.addEventListener("keydown", function(e) {
   Shiny.setInputValue("test-key_pressed", e.key, {priority: "event"});
  });'
))) # assumes shiny::NS(id) behavior is to add '{id}-' before inputId
})
