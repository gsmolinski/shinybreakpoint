test_that("js code in shinybreakpointUI is correct", {
  val <- shinybreakpointUI("test")
  expect_equal(
  val$children[[1]],
  '  let counter = 0;
  document.addEventListener("keydown", function(e) {
   Shiny.onInputChange("test-key_counted", counter++);
   Shiny.onInputChange("test-key_pressed", e.key);
  })'
) # assumes shiny::NS(id) behavior is to add '{id}-' before inputId
})
