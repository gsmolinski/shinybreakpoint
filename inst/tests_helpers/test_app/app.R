library(shiny)
library(magrittr)

options(shiny.reactlog = TRUE)

ui <- fluidPage(
  theme = bslib::bs_theme(5),
  mod1UI("mod1"),
  tags$h3("Below 'appServer' part:"),
  textInput("txt", "Text")
)

appServer <- function(input, output, session) {
  observe({
    input$txt
  }, label = "labelled observer depends on txt")
}

server <- function(input, output, session) {
  shinybreakpoint::shinybreakpointServer()
  appServer(input, output, session)
  mod1Server("mod1")
}

shinyApp(ui, server)
