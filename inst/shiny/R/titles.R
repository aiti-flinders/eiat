titleUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("title"))
}

titleServer <- function(id, region_selected, pretext = NULL, posttext = NULL) {
  moduleServer(
    id,
    function(input, output, session) {

      output$title <- renderUI({
        h3(glue('{pretext} in {region_selected()} {posttext}'))

    })
    }
  )
}
