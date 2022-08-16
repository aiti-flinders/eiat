read_industry_flow_table <- function(path) {
  suppressMessages(
    raw <- readxl::read_excel(path,
                              sheet = "Table 5",
                              .name_repair = "minimal")
  )

  colnames(raw) <- c(1:length(raw))


  ix <- match(TRUE, grepl("FROM INDUSTRY", raw[[1]]))

  df <- dplyr::slice(raw, (ix + 1):nrow(raw)) %>%
    dplyr::select(-c(1,2))

  df[1:length(df)] <- sapply(df[1:length(df)], as.numeric)

  colnames(df) <- io_cols

  df







  # ,
  #                    col_names = io_cols,
  #                    range = readxl::anchored(anchor, dim = c(127, 124), col_names = FALSE)
  #)
}

read_national_employment_table <- function(path) {
  suppressMessages(
    raw <- readxl::read_excel(path, sheet = "Table 20")
  )

  #Identify where the data begins
  df <- raw[(!is.na(raw[[2]]) & !is.na(raw[[1]])), ]

  df <- df[c(1:2, (length(df)-2):length(df))]

  colnames(df) <- c("ioig", "industry_name", "employed_full_time", "employed_part_time", "full_time_equivalent")

  df$ioig = stringr::str_pad(df$ioig, side = "left", width = 4, pad = "0")
  df[3:5] = sapply(df[3:5], as.numeric)

  df$employed_total <- df$employed_full_time + df$employed_part_time

  df[c("ioig", "industry_name", "employed_total", "full_time_equivalent")]


  dplyr::left_join(df, ioig_anzsic_div,  by = "ioig") %>%
    dplyr::group_by(anzsic_division_code) %>%
    dplyr::summarise(dplyr::across(c(employed_total, full_time_equivalent), sum), .groups = "drop") %>%
    tidyr::pivot_longer(cols = -anzsic_division_code,
                        names_to = "from_anzsic") %>%
    tidyr::pivot_wider(names_from = anzsic_division_code,
                       values_from = value) %>%
    dplyr::arrange(factor(from_anzsic, levels = c("full_time_equivalent", "employed_total"))) %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(A:S))),
                  `Total Supply` = `Total Industry Uses`)


}



# Gross Fixed Capital Formation = Private Gross Fixed Capital Formation + Public Corporations Gross Fixed Capital Formation + General Government Gross Fixed Capital Formation
# Combine all capitals, combine all taxes, close table to imports

create_114_sector <- function(path) {

  industry_industry_raw <- read_industry_flow_table(path)

  industry_industry <- industry_industry_raw[complete.cases(industry_industry_raw),] %>%
    dplyr::rename_with(.cols = c(1:114), ~names(io_cols[1:114])) %>%
    dplyr::mutate(`Gross Fixed Capital Formation` = rowSums(dplyr::across(dplyr::contains("Fixed Capital Formation"))),
                  .after = "General Government Final Consumption Expenditure") %>%
    dplyr::select(-`Private Gross Fixed Capital Formation`,
                  -`Public Corporations Gross Fixed Capital Formation`,
                  -`General Government Gross Fixed Capital Formation`,
                  -`Final Uses (Q1 to Q7)`) %>%
    dplyr::mutate(row_name = c(names(io_rows[1:114]),
                               io_rows[115:123]), .before = 1)

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

  return(industry_industry_114)
}

create_19_sector <- function(path) {

  industry_industry_114 <- create_114_sector(path)

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

  q13 <- dplyr::left_join(q1_19, q3_19) %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(2:20))), .before = "Households Final Consumption Expenditure") %>%
    dplyr::bind_rows(
      dplyr::summarise(., dplyr::across(where(is.character), ~"Total Intermediate Use") ,
                       dplyr::across(where(is.double), sum))
    )

  q24 <- dplyr::left_join(q2_19, q4_19) %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(2:20))))


  employment <- read_national_employment_table("data-raw/520905500120_0910.xls")

  industry_industry_flows_19 <- dplyr::bind_rows(q13, q24) %>%
    dplyr::mutate(`Total Supply` = rowSums(dplyr::across(c(21:26)))) %>%
    dplyr::bind_rows(employment)

  return(industry_industry_flows_19)

}
