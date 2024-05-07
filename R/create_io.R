#' Create 114 sector input-output table.
#'
#' @param update logical. FALSE (the default) to use package data, TRUE to re-download from the ABS
#'
#' @description Creates a 114 sector input-output table from the ABS National Accounts: Input-Output tables
#' Industry by industry flow table (Table 5.) and Employment by industry table (Table 20.). The 114 sector version
#' created is modified to enable easy creation of the 19 sector model used to create Regional Input-Ouput tables.
#' The current version of the data is installed with the package as `industry_flows`. A path can be specified to instead read Table 5
#' and Table 20 from a local folder. This can be useful if a different year input-output table is required.
#'
#' @examples
#' create_114_sector()
#' \dontrun{
#' create_114_sector(update = TRUE)
#' }
#'
#' @source https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/latest-release
#' @return Silently returns a list with the 114 sector input-output table ("flows"), and the path to Table 5 and Table 20.
#' @export create_114_sector
#' @importFrom stats complete.cases
#' @importFrom tidyselect where
#' @importFrom rlang .data
#'
create_114_sector <- function(update = FALSE) {

  # Method to check ABS website for a new version of Table 5 and Table 20

  if (isFALSE(update)) {

    # Use package data

    industry_flows <- industry_flows[stats::complete.cases(industry_flows), ]

  } else if (isTRUE(update)) {

    cli::cli_li("Downloading {.field Table 5. Industry by industry flow table (direct allocation of imports)} from
                {.url https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/latest-release}")

    industry_flows <- read_industry_flow_table()

    cli::cli_alert_success("Downloaded industry flows data")

    industry_flows <- industry_flows[stats::complete.cases(industry_flows), ]


    cli::cli_li("Downloading {.field Table 20. Employment by industry} from
                {.url https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/latest-release}")
    national_employment <- read_national_employment_table()
    cli::cli_alert_success("Downloaded national employment data")
  }




  industry_industry <- industry_flows %>%
    dplyr::rename_with(.cols = c(1:115), ~names(io_cols[1:115])) %>%
    dplyr::mutate(`Gross Fixed Capital Formation` = rowSums(dplyr::across(dplyr::contains("Fixed Capital Formation"))),
                  .after = "General Government Final Consumption Expenditure") %>%
    dplyr::select(-"Private Gross Fixed Capital Formation",
                  -"Public Corporations Gross Fixed Capital Formation",
                  -"General Government Gross Fixed Capital Formation",
                  -"Final Uses (Q1 to Q7)") %>%
    dplyr::mutate(row_name = c(names(io_rows[1:115]), io_rows[116:124]),
                  .before = 1)



  # Rows 1:115 in the 115 sector model are as reported in the IO table (they're the 115 sectors!)
  # Compensation of employees, GOS & Mixed Income, Total Intermediate uses are the same
  # 115 + 3 = 117
  ii_split_1 <- industry_industry %>%
    dplyr::filter(dplyr::row_number() <= 118)

  ii_split_2 <- industry_industry %>%
    dplyr::filter(!.data$row_name %in% ii_split_1$row_name) %>%
    tidyr::pivot_longer(cols = -"row_name", names_to = "industry") %>%
    tidyr::pivot_wider(id_cols = "industry", names_from = "row_name", values_from = "value") %>%
    dplyr::mutate("Taxes less subsidies on products and production" = .data$`Taxes less subsidies on products` + .data$`Other taxes less subsidies on production`,
                  .before = "Complementary imports") %>%
    dplyr::mutate("Imports" = ifelse(.data$industry %in% c("Households Final Consumption Expenditure",
                                                     "General Government Final Consumption Expenditure",
                                                     "Gross Fixed Capital Formation",
                                                     "Changes in Inventories",
                                                     "Exports of Goods and Services"),
                                     0,
                                     .data$`Complementary imports` + .data$`Competing imports`),
                  .before = "Australian Production") %>%
    dplyr::select(-"Taxes less subsidies on products",
                  -"Other taxes less subsidies on production",
                  -"Complementary imports",
                  -"Competing imports",
                  -"Value Added") %>%
    tidyr::pivot_longer(cols = -"industry",
                        names_to = "row_name") %>%
    tidyr::pivot_wider(id_cols = "row_name",
                       names_from = "industry",
                       values_from = "value")


  industry_industry_adj <- dplyr::bind_rows(ii_split_1, ii_split_2)

  ii_split_3 <- industry_industry_adj %>%
    dplyr::filter(.data$row_name %in% c("Total Intermediate Use",
                                  "Compensation of employees",
                                  "Gross operating surplus & mixed income",
                                  "Taxes less subsidies on products and production",
                                  "Imports",
                                  "Australian Production")) %>%
    tidyr::pivot_longer(cols = -"row_name",
                        names_to = "industry") %>%
    tidyr::pivot_wider(id_cols = "industry",
                       names_from = "row_name",
                       values_from = "value") %>%
    dplyr::mutate("Australian Production" = ifelse(.data$industry %in% c("Households Final Consumption Expenditure",
                                                                   "General Government Final Consumption Expenditure",
                                                                   "Gross Fixed Capital Formation",
                                                                   "Exports of Goods and Services"),
                                                   .data$`Total Intermediate Use` + .data$`Taxes less subsidies on products and production`,
                                                   .data$`Australian Production`),
                  `Australian Production` = ifelse(.data$industry == "Changes in Inventories",
                                                   .data$`Total Intermediate Use` + .data$`Taxes less subsidies on products and production`,
                                                   .data$`Australian Production`)) %>%
    tidyr::pivot_longer(cols = -"industry",
                        names_to = "row_name") %>%
    tidyr::pivot_wider(id_cols = "row_name",
                       names_from = "industry",
                       values_from = "value")

  ii_split_4 <- industry_industry_adj %>%
    dplyr::filter(!.data$row_name %in% c("Total Intermediate Use",
                                   "Compensation of employees",
                                   "Gross operating surplus & mixed income",
                                   "Taxes less subsidies on products and production",
                                   "Imports",
                                   "Australian Production"))


  industry_industry_114 <- dplyr::bind_rows(ii_split_4, ii_split_3) %>%
    dplyr::mutate("Total Supply" = ifelse(.data$row_name %in% c("Imports", "Australian Production"),
                                          rowSums(dplyr::across("Total Industry Uses":"Exports of Goods and Services")),
                                          .data$`Total Supply`))

  out <- list("flows" = industry_industry_114,
              "employment" = national_employment)



}

#' Create 19 Sector Input-Output Table.
#'
#' @param update logical. FALSE (the default) to use package data, TRUE to re-download from the ABS
#'
#'
#' @description
#' Creates a 19 Sector Input-Output Table by aggregating the 114 Sector Input-Output Table (see: `create_114_sector()`)
#'
#' @return tibble of the industry-industry flows between 19 ANZSIC Sectors for Australia.
#' @export create_19_sector
#'
#' @examples
#' create_19_sector()
create_19_sector <- function(update = FALSE) {

  create_114 <-  create_114_sector(update)

  industry_industry_114 <- create_114$flows
  employment <- create_114$employment

  q1_19 <- industry_industry_114 %>%
    tidyr::pivot_longer(cols = -"row_name",
                        names_to = 'to_ioig',
                        values_to = "flow") %>%
    dplyr::filter(dplyr::if_all(c("row_name", "to_ioig"), ~.x %in% names(io_rows[1:115]))) %>%
    dplyr::left_join(ioig_anzsic_div, by = c("row_name" = "ioig")) %>%
    dplyr::select(from_anzsic = "anzsic_division_code",
                  "to_ioig",
                  "flow") %>%
    dplyr::left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
    dplyr::select("from_anzsic",
                  to_anzsic = "anzsic_division_code",
                  "flow") %>%
    dplyr::group_by(.data$from_anzsic, .data$to_anzsic) %>%
    dplyr::summarise(flow = sum(.data$flow), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "to_anzsic", values_from = "flow")

  q2_19 <- industry_industry_114 %>%
    dplyr::filter(!.data$row_name %in% c(names(io_rows[1:115]), "Total Intermediate Use")) %>%
    dplyr::select(1:116) %>%
    tidyr::pivot_longer(cols = -"row_name",
                        names_to = "to_ioig",
                        values_to = "flow") %>%
    dplyr::left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
    dplyr::select(from_anzsic = "row_name",
                  to_anzsic = "anzsic_division_code",
                  "flow") %>%
    dplyr::group_by(.data$from_anzsic, .data$to_anzsic) %>%
    dplyr::summarise(flow = sum(.data$flow), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "to_anzsic",
                       values_from = "flow") %>%
    dplyr::arrange(factor(.data$from_anzsic, levels = c("Compensation of employees",
                                                  "Gross operating surplus & mixed income",
                                                  "Taxes less subsidies on products and production",
                                                  "Imports",
                                                  "Australian Production")))

  q3_19 <- industry_industry_114 %>%
    dplyr::filter(.data$row_name %in% names(io_rows[1:115])) %>%
    dplyr::select(c(1, 118:122)) %>%
    tidyr::pivot_longer(cols = -"row_name",
                        names_to = "to_ioig",
                        values_to = "flow") %>%
    dplyr::left_join(ioig_anzsic_div, by = c("row_name" = "ioig")) %>%
    dplyr::select(from_anzsic = "anzsic_division_code",
                  to_anzsic = "to_ioig",
                  "flow") %>%
    dplyr::group_by(.data$from_anzsic, .data$to_anzsic) %>%
    dplyr::summarise(flow = sum(.data$flow), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "to_anzsic",
                       values_from = "flow") %>%
    dplyr::select("from_anzsic",
                  "Households Final Consumption Expenditure",
                  "General Government Final Consumption Expenditure",
                  "Gross Fixed Capital Formation",
                  "Changes in Inventories",
                  "Exports of Goods and Services")

  q4_19 <- industry_industry_114 %>%
    dplyr::filter(.data$row_name %in% c("Compensation of employees", "Gross operating surplus & mixed income", "Taxes less subsidies on products and production", "Imports", "Australian Production")) %>%
    dplyr::select(c(1, 118:122)) %>%
    dplyr::rename(from_anzsic = "row_name")

  q13 <- dplyr::left_join(q1_19, q3_19, by = "from_anzsic") %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(2:20))),
                  .before = "Households Final Consumption Expenditure") %>%
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
