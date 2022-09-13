mod1UI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("show_text"), "Show text"),
    textOutput(ns("txt_output_1")),
    textOutput(ns("txt_output_2")),
    textOutput(ns("num_output"))
  )
}

mod1Server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      output$txt_output_1 <- renderPrint({
        "Text output 1"
      }) %>%
        bindEvent(input$show_text, label = "mod1-txt_output_1")

      output$txt_output_2 <- renderPrint({
        "Text output 2"
      }) %>%
        bindEvent(input$show_text)

      write_number <- reactive({
        2121
      })

      show_number <- reactive({
        write_number()
      }, label = "labelled reactive show_number")

      output$num_output <- renderPrint({
        show_number()
      })
    }
  )
}
