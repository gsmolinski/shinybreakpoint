library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(5),
)

appServer <- function(input, output, session) {

}

server <- function(input, output, session) {
  shinybreakpoint::shinybreakpointServer() # TODO: remove
  appServer(input, output, session)
}

shinyApp(ui, server)
