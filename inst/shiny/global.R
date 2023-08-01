library(eiat)
library(DT)
library(dplyr)
library(tidyr)
library(writexl)
library(shinyMatrix)
library(ggplot2)
library(aititheme)
library(glue)
library(janitor)
library(shinyvalidate)
library(shinyjs)

regions <- eiat:::get_available_regions()

description_table <- tags$table(
  tags$tr(
    tags$th("Data"),
    tags$th("Version"),
    tags$th("Last Updated")
  ),
  tags$tr(
    tags$td("Employment by Industry"),
    tags$td("2021 Census"),
    tags$td("3 October 2022")
  ),
  tags$tr(
    tags$td("National Input-Output Table"),
    tags$td("2019-20 financial year"),
    tags$td("27 June 2022")
  ),
  tags$tr(
    tags$td("Regional Input-Output Table"),
    tags$td(as.character(packageVersion('eiat'))),
    tags$td("")
  )

)

download_graph_ui <- function(id) {
  fluidRow(
    column(width = 6,
           textInput(NS(id, "filename"),
                     "Filename",
                     placeholder = "Type a filename")
    ),
    column(width = 6,
           radioButtons(NS(id, "filetype"),
                        "File extension",
                        choices = c("png",
                                    "jpeg",
                                    "pdf")
           )
    ),
    column(width = 6,
           numericInput(NS(id, "width"),
                        "Plot width (pixels)",
                        value = 1000,
                        min = 1000
           )
    ),
    column(width = 6,
           numericInput(NS(id, "height"),
                        "Plot height (pixels)",
                        value = 500,
                        min = 500
           )
    ),
    column(width = 12,
           downloadButton(NS(id, "download_plot"), "Download chart", class = "download-button"),
    )
  )
}

