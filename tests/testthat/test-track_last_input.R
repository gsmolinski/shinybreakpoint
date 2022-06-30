test_that("JS code in track_last_input is correct", {
  val <- track_last_input("test")
  expect_equal(
    val$children[[1]], tags$script(HTML(' document.addEventListener("shiny:inputchanged", function(e) {
  if (!e.startsWith("test-")) {
    Shiny.setInputValue("test-last_input", e.name);
  }
 });'))) # assumes shiny::NS(id) behavior is to add '{id}-' before inputId
})
