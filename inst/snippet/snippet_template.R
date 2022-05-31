library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(),
)

appServer <- function(input, output, session) {

}

server <- function(input, output, session) {
  appServer(input, output, session)
  shinybreakpoint::shinybreakpointServer() # TODO: remove
}

shinyApp(ui, server)
