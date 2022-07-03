server_labelled_observers <- function(input, output, session) {
  observe(label = "label1", {
    2 + 2
  })

  observeEvent({

  }, label = "label2")

  observe({

  }, label = label3)

  observe({

  }, label = my_fun("label4"))
}
