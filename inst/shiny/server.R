function(input, output, session) {


  options(DT.options = list(pageLength = 20,
                            dom = "t"))


  # File Upload -------------------------------------------------------------

  user_matrix <- reactive({

    req(input$upload)

    ext <- tools::file_ext(input$upload$name)

    uploaded_file <- switch(ext,
                            csv = try(vroom::vroom(input$upload$datapath, delim = ",")),
                            validate("Invalid file; Please upload a .csv file.")
    )

    intended_cols <- "Sector|2023|2024|2025|2026|2027|2028|2029|2030|2031|2032"


    if (any(class(uploaded_file) == "try-error")) {
      reset <- TRUE
      cols <- TRUE
    } else {

      uploaded_data <- uploaded_file %>%
        select(where(is.double)) %>%
        mutate(across(where(is.double), ~ifelse(is.na(.x), 0, .x)))

      cn <- colnames(uploaded_data)
      cols <- grepl(intended_cols, cn)
            reset <- FALSE

    }




    if (any(!cols)) {
      showModal(modalDialog(
        title = "Warning",
        "Unexpected columns identified in uploaded data.
        Input data may not be accurate.
        Please validate before continuing, or use the Excel Template."
      ))

      n_col <- ncol(uploaded_data)

      if (n_col > 10) {
        showModal(modalDialog(
          title = "Warning",
          "The data you have uploaded exceeds the 10 year limitation of the EIAT App.
          Only the first 10 years worth of data have been uploaded"
        ))

        uploaded_data <- uploaded_data[1:10]
        n_col <- 10
      }

      m <- matrix(sapply(uploaded_data, cbind, simplify = TRUE),
                  nrow = 19,
                  ncol = n_col,
                  dimnames = list(eiat:::anzsic_swap$name,
                                  2023:(2023 + n_col - 1)))

      m
    } else if (reset) {

      showModal(modalDialog(
        title = "Error",
        "The uploaded file could not be opened.
        Please check the file for any errors, or use the Excel Template.
        No data has been entered."
      ))

      m <- matrix(0,
                  nrow = 19,
                  ncol = 1,
                  dimnames = list(eiat:::anzsic_swap$name,
                                  2023))

      m

    } else {

      n_col <- ncol(uploaded_data)

      m <- matrix(sapply(uploaded_data, cbind, simplify = TRUE),
                  nrow = 19,
                  ncol = n_col,
                  dimnames = list(eiat:::anzsic_swap$name,
                                  2023:(2023 + n_col - 1)))

      m
    }

  })





  # File Downloads  ------------------------------------------------

  # Template
  output$download <- downloadHandler(
    filename = function() {
      "EIAT-Template.csv"
    },
    content = function(file) {
      out <- matrix(0, nrow = 19, ncol = input$years, dimnames = list(eiat:::anzsic_swap$name, 2023:(2023 + input$years - 1)))
      out <- as_tibble(out, rownames = "Sector")
      vroom::vroom_write(out, file, delim = ",")
    }
  )

  # Objects
  downloadServer("download_national_io", "National I-O (19 Sector) Table.csv", national_19)
  downloadServer("download_regional_io", "Regional I-O (19 Sector) Table.csv", as_tibble(lq_models[[input$lga]], rownames = "Sector"))
  downloadServer("download_regional_employment", "Regional Employment.csv", regional_employment())
  downloadServer("download_expenditure", "Expenditure Plot.png")

  # Matrix ------------------------------------------------------------------

  # Keep track of what has been typed in
  # When ADDING years - bind a matrix with all values 0 for the NEW years.
  # If initial matrix is 2022, 2023, 2024 and need to add 2 columns (from 3 years -> 5 years), new matrix
  # has 2 columns with names 2025, 2026

  v <- reactiveValues(
    upload_state =  NULL
  )

  observeEvent(input$upload, {
    v$upload_state <- "uploaded"
  })

  observeEvent(input$clear, {
    v$upload_state <- "reset"
  })

  observeEvent(input$years, {

    req(iv$is_valid())

    if (is.null(input$upload)) {
      entered <- input$industry_input
      t <- ifelse(is.null(input$years), 1, input$years - ncol(entered))
      if (t > 0) { # If we're adding years
        new_cols <- input$years - ncol(entered)
        colnames_from <- max(as.numeric(colnames(entered))) + 1
        colnames_to <- max(as.numeric(colnames(entered))) + new_cols
        new_matrix <- matrix(0,
                             nrow = 19,
                             ncol = new_cols,
                             dimnames = list(eiat:::anzsic_swap$name,  colnames_from:colnames_to)
        )
        m <- cbind(entered, new_matrix)
      } else if (t < 0) { #If we're removing years
        m <- entered[, 1:input$years, drop = FALSE]
      } else { # t == 0 on initialisation
        m <- input$industry_input
      }

    } else if (v$upload_state == "reset") {
      col_names <- 2023:(2023 + input$years - 1)

      m <- matrix(0,
             nrow = 19,
             ncol = input$years,
             dimnames = list(eiat:::anzsic_swap$name,
                             col_names))
    } else {
      m <- user_matrix()[, 1:input$years, drop = FALSE]
    }
    updateMatrixInput(session, "industry_input", m)
  })




  observeEvent(input$clear, {

    col_names <- 2023:(2023 + input$years - 1)

    m <- matrix(0, nrow = 19, ncol = input$years, dimnames = list(eiat:::anzsic_swap$name, col_names))
    shinyjs::reset("upload")
    updateMatrixInput(session, "industry_input", m)

  })

  observeEvent(input$upload, {
    updateNumericInput(session, "years", value = NCOL(user_matrix()))
    updateMatrixInput(session, "industry_input", user_matrix())
  })

  region_selected <- reactive(input$lga)
  output$region_selected <- renderText({
    region_selected()
  })

  observe({
    lga <- if (is.null(input$state)) character(0) else {
      regions %>%
        filter(state == input$state) %>%
        pull(lga) %>%
        unique() %>%
        sort()
    }
    still_selected <- isolate(input$lga[input$lga %in% lga])
    updateSelectInput(session, "lga", choices = lga, selected = first(lga))
  })







  # Modules --------------------------------------------------------------
  AnnualServer("employment", tab = "emp", reactive(input$lga), reactive(input$industry_input))
  AnnualGraphServer("employment_graph", tab = "emp", reactive(input$lga), reactive(input$industry_input))
  AnnualServer("grp", tab = "grp", reactive(input$lga), reactive(input$industry_input))
  AnnualGraphServer("grp_graph", tab = "grp", reactive(input$lga), reactive(input$industry_input))
  TotalServer("employment_total", tab = "emp", reactive(input$lga), reactive(input$industry_input))
  TotalGraphServer("employment_total_graph", tab = "emp", reactive(input$lga), reactive(input$industry_input))
  TotalServer("grp_total", tab = "grp", reactive(input$lga), reactive(input$industry_input))
  TotalGraphServer("grp_total_graph", tab = "grp", reactive(input$lga), reactive(input$industry_input))


  # Report ------------------------------------------------------------------

  ## Download report

  output$report <- downloadHandler(
    filename = function() {
      paste0(input$filename, input$report_format)
    },
    content = function(file) {
      withProgress(message = "Rendering document, please wait!", {
        tempReport <- file.path(tempdir(), c("report.Rmd", "report-doc.Rmd", "preamble-latex.tex"))

        if (input$report_format == ".pdf") {
          file.copy("report.Rmd", tempReport[1], overwrite = TRUE)
          file.copy("preamble-latex.tex", tempReport[2], overwrite = TRUE)
        } else {
          file.copy("report-doc.Rmd", tempReport[1], overwrite = TRUE)

        }

        params <- list(title = input$project_name,
                       description = input$project_desc,
                       author = input$project_analyst,
                       data = input$industry_input,
                       region = input$lga,
                       include_tables = input$report_tables,
                       include_graphs = input$report_graphs)

        rmarkdown::render(tempReport[1],
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      })
    }
  )

  ## Download data

  # Input data
  input_data <- reactive({
    as_tibble(input$industry_input, rownames = "Sector")
  })
  get_impacts <- function(impact_type, impact) {
    reactive({
      impact_analysis(input$lga,
                    input$industry_input) %>%
        .[[impact]] %>%
        filter(type == impact_type) %>%
        pivot_wider(names_from = year,
                    values_from = value)
  })
  }


  output$data <- downloadHandler(
    filename = function() {
      "EIAT Data Summary.xlsx"
    },
    content = function(file) {
      withProgress(message = "Collecting data, please wait!", {
         writexl::write_xlsx(
           list(`Input` = input_data(),
                `Employment Impacts (Direct)` = get_impacts("Direct Employment", "emp")(),
                `Employment Impacts (Flow on)` = get_impacts("Flow on Employment", "emp")(),
                `Employment Impacts (Total)` = get_impacts("Total Employment", "emp")(),
                `GRP Impacts (Direct)` = get_impacts("Direct GRP", "grp")(),
                `GRP Impacts (Flow on)` = get_impacts("Flow on GRP", "grp")(),
                `GRP Impacts (Total)` = get_impacts("Total GRP", "grp")(),
                `Regional Input Output Table` = as_tibble(lq_models[[input$lga]], rownames = "Sector")),
           file)


      })
    }
  )







  # Inputs ------------------------------------------------------------------

  inputServer("input_summary", reactive(input$lga), reactive(input$industry_input))


  # NumericInput Validation -------------------------------------------------

  iv <- InputValidator$new()
  iv$add_rule("years", sv_between(1,10))
  iv$enable()

  observeEvent(input$years, {
    if (!iv$is_valid()) {
      showNotification("The EIAT only supports economic assessments between 1 and 10 years.",
                       type = "warning")
    }
  })


  # Base Data ---------------------------------------------------------------


  output$national_io <- DT::renderDataTable(
    datatable(national_19,
              rownames = FALSE,
              options = list(pageLength = 30,
                             scrollX = TRUE)) %>%
      formatRound(columns = c(2:length(national_19)), digits = 0)
  )

  regional_employment <- reactive({
    lq_models[[input$lga]][c("Local Employment", "Total Employment"),] %>%
      as_tibble(rownames = "employment_type") %>%
      pivot_longer(-employment_type,
                   names_to = "Sector") %>%
      pivot_wider(names_from = employment_type) %>%
      mutate(across(where(is.double), round)) %>%
      filter(Sector %in% eiat:::anzsic_swap$name)
  })

  output$regional_employment <- renderDataTable(
    datatable(regional_employment(),
              rownames = FALSE)


  )

  output$regional_base_table <- renderUI({
    h3(glue("Regional Employment (FTE) in {region_selected()}"))
  })


  output$regional_io <- renderDataTable(
    datatable(lq_models[[input$lga]],
              rownames = TRUE,
              options = list(pageLength = 30,
                             scrollX = TRUE)) %>%
      formatRound(columns = c(1:27),
                  digits = 0)
  )

  output$multipliers <- renderDataTable(
    multipliers(input$region) %>%
      datatable(rownames = TRUE) %>%
      formatRound(columns = c(2:length(.)), digits = 3)
  )

}
