library(shinytest2)

dir <- system.file(package = "shinybreakpoint", "tests_helpers/test_app")

test_that("src_code for 'files' mode is displayed correctly for
          'app.R' element and 'R/module_1.R' element", {
  app <- AppDriver$new(dir, name = "test_app_files")
  app$set_inputs(`shinybreakpoint-key_pressed` = "F4", allow_no_input_binding_ = TRUE,
      priority_ = "event")
  app$set_inputs(`shinybreakpoint-app_mode` = "files", allow_no_input_binding_ = TRUE)
  app$set_inputs(`shinybreakpoint-element` = paste0(dir, "/app.R"), allow_no_input_binding_ = TRUE)
  app$expect_values(output = "shinybreakpoint-src_code")

  app$set_inputs(`shinybreakpoint-element` = paste0(dir, "/R/module_1.R"))
  app$expect_values(output = "shinybreakpoint-src_code")
})

test_that("src_code for 'last_input' mode is displayed correctly
          for 'txt' and 'mod1-show_text' ids", {
            app <- AppDriver$new(dir, name = "test_app_last_input")
            app$set_inputs(`shinybreakpoint-last_input` = "txt", allow_no_input_binding_ = TRUE)
            app$set_inputs(txt = "aaa")
            app$set_inputs(`shinybreakpoint-key_pressed` = "F4", allow_no_input_binding_ = TRUE,
                           priority_ = "event")
            app$set_inputs(`shinybreakpoint-app_mode` = "last_input", allow_no_input_binding_ = TRUE)
            app$set_inputs(`shinybreakpoint-element` = "txt", allow_no_input_binding_ = TRUE)
            app$expect_values(output = "shinybreakpoint-src_code")

            app$click("mod1-show_text")
            app$set_inputs(`shinybreakpoint-last_input` = "mod1-show_text", allow_no_input_binding_ = TRUE)
            app$set_inputs(`shinybreakpoint-key_pressed` = "F4", allow_no_input_binding_ = TRUE,
                           priority_ = "event")
            app$set_inputs(`shinybreakpoint-app_mode` = "last_input", allow_no_input_binding_ = TRUE)
            app$set_inputs(`shinybreakpoint-element` = "mod1-show_text", allow_no_input_binding_ = TRUE)
            app$expect_values(output = "shinybreakpoint-src_code")
})
