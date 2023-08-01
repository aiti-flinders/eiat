downloadUI <- function(id, button_text) {

  ns <- NS(id)

  downloadButton(ns("download"), label = button_text)

}

downloadServer <- function(id, filename, object_to_download) {

  moduleServer(
    id,
    function(input, output, session) {

      output$download <- downloadHandler(
        filename = function() {
          {{filename}}
        },
        content = function(file) {
         switch(tools::file_ext(filename),
                "csv" = vroom::vroom_write(object_to_download, file, delim = ","),
                "png" = ggsave(file, object_to_download)
         )

        }
      )



    }
  )
}

