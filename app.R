# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)

# options("golem.app.prod" = TRUE)
options("golem.app.prod" = FALSE)
run_app()

# Debug just the UI without login:

# shiny::shinyApp(
#   ui = .app_ui,
#   server = .app_server
# )
