

navbarPage("Economic Impact Assessment Tool",
           header = tags$head(
             tags$link(rel = "stylesheet", type = "text/css", href = "styling.css")
           ),
           fluid = TRUE,
           tabPanel("Description",
                    fluidRow(
                      useShinyjs(),
                      column(width = 12,
                             h1("Economic Impact Analysis Tool"),
                             p("EIAT is an online input-output (I-O) analysis tool for Local Goverment Areas across Australia.
                      It has been developed by the Australian Industrial Transformation Institute at Flinders University,
                      in conjunction with AURIN. This tool is designed to enable users to conduct regional economic impact analyses.
                      Input-Output models provide a standard approach for the estimation of the economic impact of a particular activity
                      (e.g. construction of a new infrastructure project). A regional economic impact statement regarding the impact of
                      major projects and policies has become a critical part of regional development analysis, and is an extensive component
                      of the applied economic literature. The linkages between employment opportunities and residents - and business to business linkages affect
                      urban design and transport systems, infrastructure demand, and regional taxes, amongst others."),
                      column(width = 6,
                             h2("Data Summary"),
                      p("This tool draws on data from the following sources."),
                      p(tags$b("Employment by industry: "),"2021 Census, last updated 3 October 2022"),
                      p(tags$b("National Input-Output table: "), "Australian Bureau of Statistics, last updated 27 June 2022"),
                      p(tags$b("Regional Input-Output tables: "), "Derived from the national I-O table using a
                               location quotient model. The source code is available", a(href = 'https://github.com/aiti-flinders/eiat', 'here.'),
                        "The current version is: ", as.character(packageVersion("eiat")))
                      ),
                      column(width = 6)
                    )
                    ),
                    tags$hr(),
                    fluidRow(
                      column(width = 12,
                             h1("Using the Economic Impact Analysis Tool"),
                             tags$b("The quality of an economic impact assessment is dependent on the quality of input data.", style = 'color:red'),
                             p("The analyst is required to enter the direct capital expenditures associated with the investment project of interest.
                             These expenditures must be expressed in millions of dollars ($M) in basic or producer prices which exclude margins,
                             taxes, and subsidies. Only expenditure which is expected to occur inside the region should be entered. Expenditures that
                             occur outside of the region should be excluded from the analysis. This includes expenditure which may be allocated to a region but
                             must be imported from outside the region. The tool is agnostic to input data. Any reporting of potential economic impacts
                             should also include a summary of the data used to generate the impact."),
                             p("A user guide for the EIAT is available for download", a("here.", href = "https://github.com/aiti-flinders/eiat-app/raw/main/user_guide.pdf", target = "_blank"))
                      ),
                      column(width = 6,
                             h2("Important Assumptions"),
                             p("The use of an input-output model imposes a number of assumptions which must be considered in interpreting the predicted impacts.
                             They include:",
                             tags$ol(
                               tags$li("Increases in demand in the region are serviced by industries with constant proportions, and no significant price adjustments occur."),
                               tags$li("Industries have a linear production function, which implies constant returns to scales and fixed input proportions."),
                               tags$li("Firms within a sector are homogenous, which implies they produce a fixed set of products that are not produced by any other sector,
                                       and that the input structure of the firms are the same."),
                               tags$li("The model is a static model that does not take into account the dynamic processes involved in the adjustment to an external change.")
                             )
                             )
                      ),
                      column(width = 6,
                             h2("Required Information"),
                             p("Before using input-output analysis to estimate the economic impact of regional expenditure, the user is required to collect information.
                             The analyst must know the magnitude of various expenditures and where they occur. Also needed is information on how the sectors receiving this
                             expenditure share their expenditures among the various sectors from whom they buy, and so on, for the further expenditure rounds. While private
                             and public stakeholders are welcome to use this powerful tool to conduct input-output analysis, it is recommended that expert consultants are
                             engaged for a full and detailed report on the estimations of economic impacts and the interpretations.")

                      )
                    )



           ),


           tabPanel("Project Setup",
                    tabsetPanel(
                      tabPanel("Setup and Data Input",
                               p(class = 'instructions',
                                 "1. Select the region for which you would like to conduct the economic impact assessment. Regions are Local Government Areas
                                 as represented by the Australian Statistical Geography Standard (ASGS) Edition 3."),
                               fluidRow(
                                 column(width = 6,
                                        selectInput("state", "State:", unique(regions$state))
                                 ),
                                 column(width = 6,
                                        selectInput("lga", "Region (LGA): ", unique(regions$lga))
                                 )
                               ),
                               p(class = "instructions",
                                 "2. Select the number of years over which to conduct the economic impact assessment"),
                               numericInput("years", "Number of years : ", value = 1, min = 1, max = 10, step = 1),

                               p(class = "instructions",
                                 "3. Enter input price data into the table below."),
                               p("Data can be entered directly by selecting a cell and typing the value.
                                 Input price data should be in millions of Australian dollars."),
                               p("Instructions for uploading data prepared offline can be found below the table."),
                               h3("Data Input"),

                               matrixInput("industry_input",
                                           class = "numeric",
                                           cols = list(
                                             names = TRUE,
                                             extend = TRUE,
                                             editableNames = FALSE,
                                             delta = 0,
                                             delete = TRUE
                                           ),
                                           rows = list(
                                             names = TRUE,
                                             extend = TRUE,
                                             editableNames = FALSE,
                                             delta = 0,
                                             delete = FALSE
                                           ),
                                           value = matrix(0, nrow = 19, ncol = 1, dimnames = list(eiat:::anzsic_swap$name, lubridate::year(lubridate::today())))
                               ),
                               splitLayout(
                                 downloadButton("download", "Download Excel Template", style = 'margin-top:25px'),
                                 fileInput("upload", "Upload Data", accept = ".csv")
                               ),
                               p("For more complicated analyses, you may wish to tabulate the input price data offline.
                                 The 'Download Excel Template' button will download a file called 'EIAT-Template.csv' based
                                 on the parameters set above. This file can be opened in Microsoft Excel, OpenOffice, or Google Sheets.
                                 You may rename the file if you wish, but the extension", tags$b("MUST"), "remain .csv. Once
                                 the data has been entered into the template, it can then be uploaded using the 'Upload Data' button.
                                 Please ensure that the data populated into the app is correct before you proceed."),
                               p(tags$b("There is no guarantee that data prepared without using the Excel Template will upload correctly!")),
                               p("If you have made a mistake, or would like to start again, clicking the button below will reset all
                                 values to 0."),
                               actionButton("clear", "Clear ALL Entered Data")

                      )
                    )

           ),
           tabPanel("Input Summary",
                    inputUI("input_summary")
           ),

           navbarMenu("Economic Impacts",
                      tabPanel("Employment Impacts",
                               tabsetPanel(
                                 AnnualUI("employment"),
                                 AnnualGraphUI("employment_graph"),
                                 TotalUI("employment_total"),
                                 TotalGraphUI("employment_total_graph")
                               )
                      ),
                      tabPanel("Gross Regional Product Impacts",
                               tabsetPanel(
                                 AnnualUI("grp"),
                                 AnnualGraphUI("grp_graph"),
                                 TotalUI("grp_total"),
                                 TotalGraphUI("grp_total_graph")
                               )
                      )
           ),

           navbarMenu("Base Data",
                      tabPanel("National I-O Table",
                               h3("National I-O (19 Sectors) Table"),
                               p("I-O tables are a means of presenting a detailed analysis of production and the use of products (goods and
                                 services) and the income generated in the production process for a particular period, usually one year.
                                 They show produts produced by each industry and how they are used by other industries and final users.
                                 The tables are based on the principle that the value of the output of each industry can be expressed as the sum
                                 of the values of all the inputs to that industry plus any profits made. All of the products produced by
                                 each industry are identified as being used as inputs by other industries in their production process, being sold
                                 to final users of the products or contributing to the change in inventories."),
                               downloadUI("download_national_io", "Download National I-O Table."),
                               div(DT::dataTableOutput("national_io"), style = 'font-size: 75%; width: 100%')
                      ),
                      tabPanel("Regional Employment",
                               uiOutput("regional_base_table"),
                               p("Summary of regional employment data. Data are in terms of full-time equivalent (FTE).
                                 Employment data for undefined industry and undefined place of residence and place of work were distributed over
                                 all defined regions and industry sectors according to their employment proportions.",
                                 tags$b("Local Employment"), "refers to those who live and work in the region.",
                                 tags$b("Total Employment"), "refers to those who work in the region."),
                               downloadButton("download_regional_employment", "Download Regional Employment"),
                               div(DT::dataTableOutput("regional_employment"), style = 'font-size: 75%; width: 100%')),
                      tabPanel("Regional I-O (19 Sectors) Table",
                               p("The construction of the Regional Input-Output tables were adapted to allow for the fact
                                 that employment inputs by industry can come from within the region and from outside the region.
                                 Therefore, two rows (for wages and salaries, and employment) were considered for the labour input.
                                 Labour used by the sector that work and live in the region and labour used by the sector that work in the
                                 region but resides outside the region."),
                               downloadButton("download_regional_io", "Download Regional I-O Table."),
                               div(DT::dataTableOutput("regional_io"), style = 'font-size: 75%; width: 100%')
                      )
           ),
           tabPanel("Report",
                    h3("Project Details"),
                    p("You may generate and download a report summarising your economic impact assessment."),
                    fluidRow(
                      column(width = 4,
                             textInput("project_name", "Project Name: ", placeholder = "Enter Project Name")
                      ),
                      column(width = 4,
                             textInput("project_analyst", "Project Analyst: ", placeholder = "Enter Analyst Name")
                      ),
                      column(width = 4,
                             textAreaInput("project_desc", "Project Description: ", placeholder = "Enter Project Description")

                      )
                    ),
                    h3("Download Report"),
                    fluidRow(
                      column(width = 4,
                             fluidRow(style = "margin-right:0px; margin-left:0px",
                                      textInput("filename", "Filename", placeholder = "Filename"),
                                      radioButtons("report_format", "Report Format",
                                                   choiceNames = c("PDF", "Word Document"),
                                                   choiceValues = c(".pdf", ".docx"))
                             )
                      ),
                      column(width = 4,
                             checkboxGroupInput(inputId = "report_tables",
                                                label = "Table Options",
                                                choices = c("Direct employment impacts",
                                                            "Direct GRP impacts",
                                                            "Flow-on employment impacts",
                                                            "Flow-on GRP impacts",
                                                            "Total employment impacts",
                                                            "Total GRP impacts",
                                                            "Summary employment impacts",
                                                            "Summary GRP impacts"),
                                                selected = c("Summary employment impacts", "Summary GRP impacts"))
                      ),
                      column(width = 4,
                             checkboxGroupInput(inputId = "report_graphs",
                                                label = "Graph Options",
                                                choices = c("Summary input data",
                                                            "Employment impacts",
                                                            "GRP impacts"),
                                                selected = c("Summary input data", "Employment impacts", "GRP impacts"))
                      ),
                      column(width = 12,
                             downloadButton("report", "Generate Report"),
                             downloadButton("data", "Download All Data")
                      )
                    )
           )
)


