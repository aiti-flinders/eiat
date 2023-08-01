TotalUI <- function(id) {

  ns <- NS(id)

  tabPanel("Total Impact Results",
           uiOutput(ns("title")),
           dataTableOutput(ns("total_table")),
  )

}

TotalServer <- function(id, tab, region, impact) {

  moduleServer(
    id,
    function(input, output, session) {

      impact_data <- reactive({
        impact_analysis(region = region(),
                        impacts = impact()) %>%
          .[[tab]]
      })

      output$title <- renderUI({
        switch(tab,
               "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}")),
               "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}")))
      })

      output$total_table <- renderDataTable({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts. ")
        }

        if (tab == "emp") {
          disp <- function(table) {
            formatRound(table, 2:4, digits = 1)
          }
        } else {
          disp <- function(table) {
            formatCurrency(table, 2:4, currency = "$", before = TRUE, digits = 1)
          }
        }


        impact_data() %>%
          group_by(Sector, type) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          mutate(Sector = factor(Sector, levels = rownames(impact()))) %>%
          arrange(Sector) %>%
          pivot_wider(id_cols = c(Sector),
                      names_from = type,
                      values_from = value) %>%
          select(Sector, contains(c("Direct",
                                    "Flow on",
                                    "Total"))) %>%
          janitor::adorn_totals(fill = "Total") %>%
          datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"),
                    rownames = FALSE,
                    extensions = "Buttons",
                    options = list(dom = "Bt",
                                   buttons = c("copy", "csv", "excel", "pdf", "print"))) %>%
          disp()
      })




    }
  )
}

TotalGraphUI <- function(id) {
  ns <- NS(id)

  tabPanel("Total Impact Graphs",
           uiOutput(ns("title")),
           plotOutput(height = '500px', ns("total_plot")),
           download_graph_ui(id))

}

TotalGraphServer <- function(id, tab, region, impact) {
  moduleServer(
    id,
    function(input, output, session) {

      impact_data <- reactive({


        impact_analysis(region = region(),
                        impacts = impact()) %>%
          .[[tab]]
      })

      output$title <- renderUI({
        switch(tab,
               "emp" = h3(glue("Total Employment Impacts (FTE) in {region()}")),
               "grp" = h3(glue("Total Gross Regional Product Impacts ($M) in {region()}")))
      })

      create_plot <- reactive({

        if (tab == "emp") {
          title <- glue("Total Employment Impacts (FTE) in {region()}")
          fct_lvls <- c("Flow on Employment", "Direct Employment")
        } else {
          title <- glue("Total Gross Regional Product Impacts ($M) in {region()}")
          fct_lvls <- c("Flow on GRP", "Direct GRP")
        }
        impact_data() %>%
          filter(grepl("Direct|Flow on", type)) %>%
          group_by(year, type) %>%
          summarise(value = sum(value), .groups = "drop") %>%
          ggplot(aes(x = factor(year), y = value, fill = type)) +
          geom_col() +
          theme_aiti(flipped = FALSE, colour = "grey") +
          labs(x = NULL,
               y = NULL,
               title = title) +
          scale_fill_aiti()
      })

      output$total_plot <- renderPlot({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts. ")
        }

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

    }
  )
}
