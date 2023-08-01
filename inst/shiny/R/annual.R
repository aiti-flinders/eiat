AnnualUI <- function(id) {

  ns <- NS(id)

  tabPanel("Annual Impact Results",
           uiOutput(ns("title")),
           uiOutput(ns("year_radio")),
           dataTableOutput(ns("annual_table")),
  )

}

AnnualServer <- function(id, tab, region, impact) {

  moduleServer(
    id,
    function(input, output, session) {

      impact_data <- reactive({


        impact_analysis(region = region(),
                        impacts = impact()) %>%
          .[[tab]]
      })

      output$title <- renderUI({

        if (is.null(input$year_radio)) {
          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}")))

        } else if (length(input$year_radio) == 1) {

        switch(tab,
               "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")),
               "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {input$year_radio}")))
        } else {

          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {paste0(input$year_radio, collapse = ', ')}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {paste0(input$year_radio, collapse = ', ')}")))

        }


      })

      output$year_radio <- renderUI({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts.")
        }
        choices <- 2023:(2023 + ncol(impact()) - 1)
        checkboxGroupInput(inputId = session$ns("year_radio"),
                     label = NULL,
                     selected = first(choices),
                     choices = choices,
                     inline = TRUE)
      })

      output$annual_table <- renderDataTable({

        req(input$year_radio)


        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts. ")
        }

        if (tab == "emp") {

          cols <- ifelse(length(input$year_radio) == 1, 4, (2 + length(input$year_radio)) - 1)

          disp <- function(table) {
            formatRound(table, 2:cols, digits = 1)
          }
        } else {

          cols <- ifelse(length(input$year_radio) == 1, 4, (2 + length(input$year_radio)) - 1)

          disp <- function(table) {
            formatCurrency(table, 2:cols, currency = "$", before = TRUE, digits = 1)
          }
        }

        if (length(input$year_radio) <= 1) {

        impact_data() %>%
          filter(year == input$year_radio) %>%
          pivot_wider(id_cols = c(year, Sector),
                      names_from = type,
                      values_from = value) %>%
          select(Sector, contains(c("Direct",
                                    "Flow on",
                                    "Total"))) %>%
          janitor::adorn_totals() %>%
          datatable(colnames = c("Sector", "Direct", "Flow-on", "Total"),
                    rownames = FALSE,
                    extensions = "Buttons",
                    options = list(dom = "Bt",
                                   buttons = c("copy", "csv", "excel", "pdf", "print"))) %>%
          disp()

        } else if (length(input$year_radio) >= 2) {

          impact_data() %>%
            filter(grepl("Total", type),
                   year %in% input$year_radio) %>%
            pivot_wider(names_from = year,
                        values_from = value) %>%
            select(-type) %>%
            janitor::adorn_totals() %>%
            datatable(rownames = FALSE,
                      extensions = "Buttons",
                      options = list(dom = "Bt",
                                     buttons = c("copy", "csv", "excel", "pdf", "print"))) %>%
            disp()

        }
      })




    }
  )
}

AnnualGraphUI <- function(id) {
  ns <- NS(id)

  tabPanel("Annual Impact Graphs",
           uiOutput(ns("title")),
           uiOutput(ns("year_radio")),
           plotOutput(height = '500px',ns("annual_plot")),
           download_graph_ui(id)
  )

}

AnnualGraphServer <- function(id, tab, region, impact) {

  moduleServer(
    id,
    function(input, output, session)  {

      impact_data <- reactive({
        impact_analysis(region = region(),
                        impacts = impact()) %>%
          .[[tab]]
      })

      output$title <- renderUI({

        if (is.null(input$year_radio)) {
          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}")))

        } else if (length(input$year_radio) == 1) {

          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {input$year_radio}")))
        } else {

          switch(tab,
                 "emp" = h3(glue("Employment Impacts by Industry (FTE) in {region()}: {paste0(input$year_radio, collapse = ', ')}")),
                 "grp" = h3(glue("Gross Regional Product Impacts by Industry ($M) in {region()}: {paste0(input$year_radio, collapse = ', ')}")))

        }

      })

      output$year_radio <- renderUI({

        if (all(impact() == 0)) {
          validate(FALSE)
        }
        choices <- 2023:(2023 + ncol(impact()) - 1)
        checkboxGroupInput(inputId = session$ns("year_radio"),
                     selected = 2023,
                     label = NULL,
                     choices = choices,
                     inline = TRUE)
      })

      create_plot <- reactive({

        if (tab == "emp") {
          if (length(input$year_radio) == 1) {
          x_axis <- scale_x_continuous(labels = scales::comma_format(),
                                       expand = expansion(mult = c(0, 0.1)))
          title <-  glue("Employment Impacts by Industry (FTE) in {region()}: {input$year_radio}")
          fct_lvls <- c("Flow on Employment", "Direct Employment")

          } else {
            x_axis <- scale_x_discrete()
            title <- glue("Employment Impacts by Industry (FTE) in {region()}: {paste0(input$year_radio, collapse = ', ')}")
          }


        } else {
          if (length(input$year_radio) == 1) {
            x_axis <- scale_x_continuous(labels = scales::dollar_format(suffix = "M"),
                                         expand = expansion(mult = c(0, 0.1)))
            title <- glue("Gross Regional Product Impacts by Industry ($M): {region()} {input$year_radio}")
            fct_lvls <- c("Flow on GRP", "Direct GRP")
          } else {
            x_axis <- scale_x_discrete()
            title <- glue("Gross Regional Product Impacts by Industry ($M): {region()} {paste0(input$year_radio, collapse = ',')}")
          }


        }

        if (length(input$year_radio) == 1) {

        impact_data() %>%
          filter(grepl("Direct|Flow on", type),
                 year == input$year_radio)  %>%
          mutate(type = factor(type, levels = fct_lvls)) %>%
          ggplot(aes(x = value, y = reorder(Sector, value))) +
          geom_col(aes(fill = type)) +
          labs(x = NULL,
               y = NULL,
               title = title) +
          x_axis +
          theme_aiti(base_size = 15,
                     colour = "grey",
                     flipped = TRUE) +
          scale_fill_aiti()
        } else {

          impact_data() %>%
            filter(grepl("Total", type),
                   year %in% input$year_radio) %>%
            mutate(year = factor(year)) %>%
            ggplot(aes(x = year, y = value)) +
            geom_col(aes(fill = Sector)) +
            labs(x = NULL,
                 y = NULL,
                 title = title) +
            x_axis +
            theme_aiti(base_size = 15,
                       colour = "grey",
                       flipped = F)

        }
      })

      output$annual_plot <- renderPlot({

        if (all(impact() == 0)) {
          validate("Enter data in Project Setup to calculate economic impacts.")
        } else if (is.null(input$year_radio)) {
          validate("Select at least one year to show economic impacts.")
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
