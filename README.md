
<!-- README.md is generated from README.Rmd. Please edit that file -->

## State of 14.03.2022

<!-- badges: start -->
<!-- badges: end -->

### Overview

Package allows to set breakpoint (by putting ‘browser()’) in the
reactive context (reactives, observers, output renders). However, some
requirements must be met for these objects:

-   they need to live inside named function(s).
-   these named functions needs to be use inside object passed as an
    argument to the `server` parameter in `shinyApp()` function.
-   it is not possible to set breakpoint directly in the function passed
    to the `server` parameter. In other words, breakpoint can be set
    only in reactive context nested in named functions if these
    functions are then call in object passed to the `server` parameter.

### Installation

You can install the development version of shinybreakpoint from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsmolinski/shinybreakpoint")
```

### Example

Press `"F1"` to open the modal dialog and then set the breakpoint on the
line where is `input$num` and push `Activate` button - you will be in
the debug mode in your IDE.

**app.R**

``` r
library(shiny)

ui <- fluidPage(
  modUI("mod")
)

server <- function(input, output, session) {
  shinybreakpoint::shinybreakpointServer()
  modServer("mod")
}

shinyApp(ui, server)
```

**R/mod.R**

``` r
modUI <- function(id) {
  ns <- NS(id)
  tagList(
    numericInput(ns("num"), "Num", 0)
  )
}

modServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        input$num
      })
    }
  )
}
```
