library(shiny)
library(magrittr)

options(shiny.reactlog = TRUE)

ui <- fluidPage(
  theme = bslib::bs_theme(5),
  mod1UI("mod1")
)

appServer <- function(input, output, session) {

}

server <- function(input, output, session) {
  shinybreakpoint::shinybreakpointServer()
  appServer(input, output, session)
  mod1Server("mod1")
}

shinyApp(ui, server)
