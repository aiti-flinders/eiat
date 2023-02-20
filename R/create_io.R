#' Create 114 sector input-output table.
#'
#' @description Creates a 114 sector input-output table from the ABS National Accounts: Input-Output tables
#' Industry by industry flow table (Table 5.) and Employment by industry table (Table 20.). The 114 sector version
#' created is modified to enable easy creation of the 19 sector model used to create Regional Input-Ouput tables.
#' The current version of the data is installed with the package as `industry_flows`. A path can be specified to instead read Table 5
#' and Table 20 from a local folder. This can be useful if a different year input-output table is required.
#'
#' @param path path to a folder containing two spreadsheets downloaded from the Australian Bureau of Statistics.
#' NULL by default which uses the `industry_flows` data installed with the package.
#' \itemize{
#' \item{Table 5. Industry by industry flow table}{}
#' \item{Table 20. Employment by industry}{}
#' }
#'
#' @examples
#' create_114_sector()
#' create_114_sector(path = "local_data")
#'
#' @source https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/latest-release
#' @return Silently returns a list with the 114 sector input-output table ("flows"), and the path to Table 5 and Table 20.
#' @export create_114_sector
#'
create_114_sector <- function(path = NULL) {

  # Need a folder with table 5 (industry flows) and table 20 (employment by industry)
  if (is.null(path)) {

    industry_flows <- industry_flows[complete.cases(industry_flows), ]
    path_to_table_5 <- NULL
    path_to_table_20 <- NULL

  } else if (dir.exists(path)) {

    # Check for table 5. Assume that the file is always called "520905500105", either .xls or .xlsx

    path_to_table_5 <- c(paste0(path, "/", "520905500105.xls"), paste0(path, "/", "520905500105.xlsx"))
    path_to_table_5 <- path_to_table_5[which(file.exists(path_to_table_5))]

    path_to_table_20 <- c(paste0(path, "/", "520905500120.xls"), paste0(path, "/", "520905500120.xlsx"))
    path_to_table_20 <- path_to_table_20[which(file.exists(path_to_table_20))]

    if (any(file.exists(path_to_table_5))) {

      industry_flows <- read_industry_flow_table(path = path_to_table_5)
      industry_flows <- industry_flows[complete.cases(industry_flows), ]


      message(glue::glue("Reading {path_to_table_5[which(file.exists(path_to_table_5))]}"))
    } else {

      stop("Could not find `520905500105.xls/x` in `path`")

    }

    if (any(file.exists(path_to_table_20))) {

      national_employment <- read_national_employment_table(path = path_to_table_20)

      message(glue::glue("Reading {path_to_table_20[which(file.exists(path_to_table_20))]}"))
    } else {

      stop("Could not find `520905500120.xls/x` in `path`")

    }

  } else if (!is.null(path) & !dir.exists(path)) {

    stop("`path` must specify an existing folder containing the ABS National Accounts tables 5 and 20")
  }


  industry_industry <- industry_flows %>%
    dplyr::rename_with(.cols = c(1:114), ~names(io_cols[1:114])) %>%
    dplyr::mutate(`Gross Fixed Capital Formation` = rowSums(dplyr::across(dplyr::contains("Fixed Capital Formation"))),
                  .after = "General Government Final Consumption Expenditure") %>%
    dplyr::select(-`Private Gross Fixed Capital Formation`,
                  -`Public Corporations Gross Fixed Capital Formation`,
                  -`General Government Gross Fixed Capital Formation`,
                  -`Final Uses (Q1 to Q7)`) %>%
    dplyr::mutate(row_name = c(names(io_rows[1:114]), io_rows[115:123]),
                  .before = 1)



  # Rows 1:114 in the 114 sector model are as reported in the IO table (they're the 114 sectors!)
  # Compensation of employees, GOS & Mixed Income, Total Intermediate uses are the same
  # 114 + 3 = 117
  ii_split_1 <- industry_industry %>%
    dplyr::filter(dplyr::row_number() <= 117)

  ii_split_2 <- industry_industry %>%
    dplyr::filter(!row_name %in% ii_split_1$row_name) %>%
    tidyr::pivot_longer(cols = -row_name, names_to = "industry") %>%
    tidyr::pivot_wider(industry, names_from = row_name, values_from = value) %>%
    dplyr::mutate("Taxes less subsidies on products and production" = `Taxes less subsidies on products` + `Other taxes less subsidies on production`,
                  .before = "Complementary imports") %>%
    dplyr::mutate("Imports" = ifelse(industry %in% c("Households Final Consumption Expenditure",
                                                     "General Government Final Consumption Expenditure",
                                                     "Gross Fixed Capital Formation",
                                                     "Changes in Inventories",
                                                     "Exports of Goods and Services"),
                                     0,
                                     `Complementary imports` + `Competing imports`),
                  .before = "Australian Production") %>%
    dplyr::select(-`Taxes less subsidies on products`,
                  -`Other taxes less subsidies on production`,
                  -`Complementary imports`,
                  -`Competing imports`,
                  -`Value Added`) %>%
    tidyr::pivot_longer(cols = -industry,
                        names_to = "row_name") %>%
    tidyr::pivot_wider(row_name, names_from = industry, values_from = value)


  industry_industry_adj <- dplyr::bind_rows(ii_split_1, ii_split_2)

  ii_split_3 <- industry_industry_adj %>%
    dplyr::filter(row_name %in% c("Total Intermediate Use",
                                  "Compensation of employees",
                                  "Gross operating surplus & mixed income",
                                  "Taxes less subsidies on products and production",
                                  "Imports",
                                  "Australian Production")) %>%
    tidyr::pivot_longer(cols = -row_name, names_to = "industry") %>%
    tidyr::pivot_wider(industry, names_from = row_name, values_from = value) %>%
    dplyr::mutate("Australian Production" = ifelse(industry %in% c("Households Final Consumption Expenditure",
                                                                   "General Government Final Consumption Expenditure",
                                                                   "Gross Fixed Capital Formation",
                                                                   "Exports of Goods and Services"),
                                                   `Total Intermediate Use` + `Taxes less subsidies on products and production`,
                                                   `Australian Production`),
                  `Australian Production` = ifelse(industry == "Changes in Inventories",
                                                   `Total Intermediate Use` + `Taxes less subsidies on products and production`,
                                                   `Australian Production`)) %>%
    tidyr::pivot_longer(cols = -industry,
                        names_to = "row_name") %>%
    tidyr::pivot_wider(row_name, names_from = industry, values_from = value)

  ii_split_4 <- industry_industry_adj %>%
    dplyr::filter(!row_name %in% c("Total Intermediate Use",
                                   "Compensation of employees",
                                   "Gross operating surplus & mixed income",
                                   "Taxes less subsidies on products and production",
                                   "Imports",
                                   "Australian Production"))


  industry_industry_114 <- dplyr::bind_rows(ii_split_4, ii_split_3) %>%
    dplyr::mutate("Total Supply" = ifelse(row_name %in% c("Imports", "Australian Production"),
                                          rowSums(dplyr::across("Total Industry Uses":"Exports of Goods and Services")),
                                          `Total Supply`))

  out <- list("flows" = industry_industry_114,
              "io_path" = path_to_table_5,
              "industry_employment_path" = path_to_table_20)


}

#' Create 19 Sector Input-Output Table.
#'
#' @description
#' Creates a 19 Sector Input-Output Table by aggregating the 114 Sector Input-Output Table (see: `create_114_sector()`)
#'
#' @return tibble of the industry-industry flows between 19 ANZSIC Sectors for Australia.
#' @export create_19_sector
#'
#' @examples
#' create_19_sector()
create_19_sector <- function(path = NULL) {

  create_114 <-  create_114_sector(path)

  industry_industry_114 <- create_114$flows
  path_to_table_20 <- create_114$industry_employment_path

  if (is.null(path_to_table_20)) {
    employment <- national_employment
  } else {employment <- read_national_employment_table(path = path_to_table_20)}

  q1_19 <- industry_industry_114 %>%
    tidyr::pivot_longer(-row_name,
                        names_to = 'to_ioig',
                        values_to = "flow") %>%
    dplyr::filter(dplyr::if_all(c(row_name, to_ioig), ~.x %in% names(io_rows[1:114]))) %>%
    dplyr::left_join(ioig_anzsic_div, by = c("row_name" = "ioig")) %>%
    dplyr::select(from_anzsic = anzsic_division_code, to_ioig, flow) %>%
    dplyr::left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
    dplyr::select(from_anzsic, to_anzsic = anzsic_division_code, flow) %>%
    dplyr::group_by(from_anzsic, to_anzsic) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = to_anzsic, values_from = flow)

  q2_19 <- industry_industry_114 %>%
    dplyr::filter(!row_name %in% c(names(io_rows[1:114]), "Total Intermediate Use")) %>%
    dplyr::select(1:115) %>%
    tidyr::pivot_longer(-row_name,
                        names_to = "to_ioig",
                        values_to = "flow") %>%
    dplyr::left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
    dplyr::select(from_anzsic = row_name, to_anzsic = anzsic_division_code, flow) %>%
    dplyr::group_by(from_anzsic, to_anzsic) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = to_anzsic,
                       values_from = flow) %>%
    dplyr::arrange(factor(from_anzsic, levels = c("Compensation of employees",
                                                  "Gross operating surplus & mixed income",
                                                  "Taxes less subsidies on products and production",
                                                  "Imports",
                                                  "Australian Production")))

  q3_19 <- industry_industry_114 %>%
    dplyr::filter(row_name %in% names(io_rows[1:114])) %>%
    dplyr::select(c(1, 117:121)) %>%
    tidyr::pivot_longer(cols = -row_name,
                        names_to = "to_ioig",
                        values_to = "flow") %>%
    dplyr::left_join(ioig_anzsic_div, by = c("row_name" = "ioig")) %>%
    dplyr::select(from_anzsic = anzsic_division_code, to_anzsic = to_ioig, flow) %>%
    dplyr::group_by(from_anzsic, to_anzsic) %>%
    dplyr::summarise(flow = sum(flow), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = to_anzsic,
                       values_from = flow) %>%
    dplyr::select(from_anzsic,
                  "Households Final Consumption Expenditure",
                  "General Government Final Consumption Expenditure",
                  "Gross Fixed Capital Formation",
                  "Changes in Inventories",
                  "Exports of Goods and Services")

  q4_19 <- industry_industry_114 %>%
    dplyr::filter(row_name %in% c("Compensation of employees", "Gross operating surplus & mixed income", "Taxes less subsidies on products and production", "Imports", "Australian Production")) %>%
    dplyr::select(c(1, 117:121)) %>%
    dplyr::rename(from_anzsic = row_name)

  q13 <- dplyr::left_join(q1_19, q3_19, by = "from_anzsic") %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(2:20))), .before = "Households Final Consumption Expenditure") %>%
    dplyr::bind_rows(
      dplyr::summarise(., dplyr::across(where(is.character), ~"Total Intermediate Use") ,
                       dplyr::across(where(is.double), sum))
    )

  q24 <- dplyr::left_join(q2_19, q4_19, by = "from_anzsic") %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(2:20))))

  industry_industry_flows_19 <- dplyr::bind_rows(q13, q24) %>%
    dplyr::mutate(`Total Supply` = rowSums(dplyr::across(c(21:26)))) %>%
    dplyr::bind_rows(employment) %>%
    tidyr::replace_na(list(`Households Final Consumption Expenditure` = 0,
                           `General Government Final Consumption Expenditure` = 0,
                           `Gross Fixed Capital Formation` = 0,
                           `Changes in Inventories` = 0,
                           `Exports of Goods and Services` = 0))



  industry_industry_flows_19$from_anzsic[1:19] <- anzsic_swap$name
  colnames(industry_industry_flows_19)[1:20] <- c("Industry Sector", anzsic_swap$name)

  return(industry_industry_flows_19)


}
