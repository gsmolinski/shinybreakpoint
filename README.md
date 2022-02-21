
<!-- README.md is generated from README.Rmd. Please edit that file -->

## State of 21.02.2022

<!-- badges: start -->
<!-- badges: end -->

### Overview

Package allows to set breakpoint (by putting ‘browser()’) in the
reactive context (reactives, observers, output renders). However, some
requirements must be met for these objects: \* they need to live inside
named function(s). \* these named functions needs to be use inside
object passed as an argument to the `server` parameter in `shinyApp()`
function. \* it is not possible to set breakpoint directly in the
function passed to the `server` parameter. In other words, breakpoint
can be set only in reactive context nested in named functions if these
functions are then call in object passed to the `server` parameter.

### Installation

You can install the development version of shinybreakpoint from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsmolinski/shinybreakpoint")
```

### MRE

**app.R**

``` r
library(shiny)
library(magrittr)

ui <- fluidPage(
  modUI("mod"),
  actionButton("debug", "Debug")  
)

server <- function(input, output, session) {
  modServer("mod")
  observe({
    obj <- shinybreakpoint:::find_object("R/mod.R", 13, environment(modServer))
    shinybreakpoint:::put_browser(obj)
  }) %>% 
    bindEvent(input$debug)
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
