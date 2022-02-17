fun1 <- function() {
  shiny::observe({
    "x"
  })
}

fun2 <- function() {
  fun3 <- function() {
    observe({
      "o"
    })
  }
}

fun4 <- function() {
  function() {
    observe({
      "!"
    })
  }
}

observe({
  "y"
})

z <- shiny::reactive({
  "z"
})

function() {observe("e")}

server <- function(input, output, session) {
  test3 <- reactive({
    "a"
  })
  observe({
    assign("parse_data", getParseData(environment(server)$server, includeText = NA),
           envir = .GlobalEnv)
    test3()
  })

  observeEvent(input$test1, {
    observe({
      "c"
    })
    observe({
      "d"
    })

  })
  reactive({
    "e"
  }) -> test4
  eventReactive(input$test1, {
    output$test2 <- renderPrint({
      test4()
      "Do not nest reactives."
    })
  })
}
