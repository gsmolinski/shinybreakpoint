server_labelled_objects <- function(input, output, session) {
  observe(label = "label1", {
    2 + 2
  })

  observeEvent({

  }, label = "label2")

  observe({

  }, label = label3)

  observe({

  }, label = my_fun("label4"))

  reactive({

  }, label = "label5")

  eventReactive({

  }, label = "label6")

  output$nothing <- renderPrint({

  }) %>%
    bindEvent(label = "label7")

  output$nothing_again <- renderPrint({

  }) %>%
    bindEvent(label = "nothing_again")
}
