fun1 <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      observe({
        reactable::reactable(src_data,
                             columns = list(col = reactable::colDef(cell = function(value) {
                               if (TRUE) {
                                 my_fun(value)
                               } else {
                                 value
                               }
                             }
                             )),
                             highlight = TRUE)
      })

      observe({
        if (TRUE) {
          2
        }
      })
    }
  )
}
