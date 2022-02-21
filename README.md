
<!-- README.md is generated from README.Rmd. Please edit that file -->

## State of 21.02.2022

<!-- badges: start -->
<!-- badges: end -->

### Overview

Package allows to set breakpoint (by putting ‘browser()’) in the
reactive context (reactives, observers, output rendering) if these
objects live inside the functions. To make it work, these functions
should be used inside ‘server’ part, i.e. inside the object passed to
the **server** parameter in `shinyApp(ui, server)`. If breakpoint will
be used directly in the function passed as a **server**, it won’t work,
i.e. it is needed to set breakpoint in the reactive context which lives
in function which then will be used in function passed as a **server**.

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
