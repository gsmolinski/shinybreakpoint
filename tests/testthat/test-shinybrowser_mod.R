test_that("js code is correct", {
  text <- shinybrowserUI("test")
  expect_equal(
  text$children[[1]],
  'document.addEventListener("keydown", function(e) {
 Shiny.onInputChange(test-key_pressed, e.key);
})'
) # assumes shiny::NS(id) behavior is to add '{id}-' before inputId
})
