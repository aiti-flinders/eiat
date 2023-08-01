inputUI <- function(id) {
  ns <- NS(id)

  tabsetPanel(
    tabPanel("Summary Tables",
             uiOutput(ns("title1")),
             dataTableOutput(ns("input_table")),
             uiOutput(ns('title2')),
             dataTableOutput(ns("input_table_sector")),
    ),
    tabPanel("Summary Graphs",
             selectInput(ns("input_type"),
                         label = NULL,
                         choices = c("Direct Capital Expenditure", "Direct Capital Expenditure by Industry")),
             plotOutput(ns("plot"), height = "500px"),
             h3("Download Graphs"),
             download_graph_ui(id)

    )
  )

}

inputServer <- function(id, region, impact) {
  moduleServer(
    id,
    function(input, output, session) {

      create_plot <- reactive({
        if (all(impact() == 0)) {
          validate("Please enter data in Project Setup to continue. ")
        }
        validate(need(input$input_type, message = FALSE))
        if (input$input_type == "Direct Capital Expenditure") {
          impact() %>%
            as_tibble(rownames = "sector") %>%
            pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
            group_by(year) %>%
            summarise(expenditure = sum(expenditure), .groups = "drop") %>%
            ggplot(aes(x = as.factor(year), y = expenditure)) +
            geom_col() +
            theme_aiti(flipped = FALSE) +
            labs(x = NULL,
                 y = NULL,
                 title = glue("Direct Capital Expenditure ($M): {region()}")) +
            scale_y_continuous(labels = scales::dollar_format(suffix = "M", accuracy = 0.1))
        } else {
          impact() %>%
            as_tibble(rownames = "sector") %>%
            pivot_longer(-sector, names_to = "year", values_to = "expenditure") %>%
            filter(expenditure != 0) %>%
            ggplot(aes(x = as.factor(year), y = expenditure, fill = sector)) +
            geom_col() +
            theme_aiti(flipped = FALSE) +
            labs(x = NULL,
                 y = NULL,
                 title = glue("Direct Capital Expenditure ($M) by Industry in {region()}")) +
            scale_y_continuous(labels = scales::dollar_format(suffix = "M", accuracy = 0.1)) +
            guides(fill = guide_legend(nrow = 2))

        }
      })



      output$plot <- renderPlot({
        create_plot()
      })

      output$download_plot <- downloadHandler(
        filename = function() {

          fname <- ifelse(is.null(input$filename), "eiat-download", input$filename)

          paste0(fname, ".", input$filetype)
        },
        content = function(file)
          ggsave(filename = file,
                 plot = create_plot() + labs(caption = paste0("Produced with EIAT Version: ", as.character(packageVersion("eiat")))),
                 dpi = "screen",
                 device = input$filetype,
                 units = "px",
                 width = input$width,
                 height = input$height)
      )



      output$input_table <- renderDataTable({
        if (all(impact() == 0)) {
          validate("Please enter data in Project Setup to continue. ")
        }
        impact() %>%
          as_tibble(rownames = "sector") %>%
          summarise(across(where(is.double), sum)) %>%
          mutate(Region = region(),
                 .before = 1) %>%
          datatable(rownames = FALSE,
                    extensions = "Buttons",
                    options = list(dom = "Bt",
                                   buttons = c("copy", "csv", "excel", "pdf", "print")))

      })


      output$input_table_sector <- renderDataTable({

        if (all(impact() == 0)) {
          validate("Please enter data in Project Setup to continue. ")
        }

        impact() %>%
          as_tibble(rownames = "Industry Sector") %>%
          filter(rowSums(across(where(is.double))) != 0) %>%
          adorn_totals() %>%
         datatable(rownames = FALSE,
                   extensions = "Buttons",
                   options = list(dom = "Bt",
                                  buttons = c("copy", "csv", "excel", "pdf", "print")))
      })



      output$title1 <- renderUI({
        validate(need(any(impact() != 0), message = F))
        h1(glue("Direct Capital Expenditure ($M) in {region()}"))
      })
      output$title2 <- renderUI({
        validate(need(any(impact() != 0), message = F))

        h1(glue("Direct Capital Expenditure ($M) by Industry in {region()}"))
      })

      downloadServer("download_input_industry_table", "Input Table (Industry).csv", input_table_sector())

    }
  )
}
