read_industry_flow_table <- function(path = NULL) {

  if (!is.null(path)) {

    raw <- suppressMessages(readxl::read_excel(path, sheet = "Table 5", .name_repair = "minimal"))

  } else if (is.null(path)) {

    # The path that the flow table is downloaded to must exist

    if (!dir.exists(here::here("data-raw"))) {
      dir.create(here::here("data-raw"))
    }

    flow_table <- readabs::download_abs_data_cube(catalogue_string = "australian-national-accounts-input-output-tables",
                                                  cube = "520905500105.xlsx",
                                                  path = here::here("data-raw"))

    raw <- suppressMessages(readxl::read_excel(flow_table, sheet = "Table 5", .name_repair = "minimal"))

  }


  colnames(raw) <- as.character(c(1:length(raw)))

  ix <- match(TRUE, grepl("FROM INDUSTRY", raw[[1]]))

  industry_flow <- raw[(ix + 1):nrow(raw), 3:ncol(raw)]

  industry_flow[1:length(industry_flow)] <- sapply(industry_flow[1:length(industry_flow)], as.numeric)


  colnames(industry_flow) <- io_cols

  industry_flow

}

read_national_employment_table <- function(path = NULL) {

  if (!is.null(path)) {
    raw <- suppressMessages(readxl::read_excel(path, sheet = "Table 20", .name_repair = "minimal"))

  } else if (is.null(path)) {

    # The path that the flow table is downloaded to must exist

    if (!dir.exists(here::here("data-raw"))) {
      dir.create(here::here("data-raw"))
    }

    employment_table <- readabs::download_abs_data_cube(catalogue_string = "australian-national-accounts-input-output-tables",
                                                        cube = "520905500120.xlsx",
                                                        path = here::here("data-raw"))

    raw <- suppressMessages(readxl::read_excel(employment_table, sheet = "Table 20"))

  }


  df <- raw[(!is.na(raw[[2]]) & !is.na(raw[[1]])), ]

  df <- df[c(1:2, (length(df) - 2):length(df))]

  colnames(df) <- c("ioig", "industry_name", "employed_full_time", "employed_part_time", "FTE Employment")

  df$ioig = stringr::str_pad(df$ioig, side = "left", width = 4, pad = "0")
  df[3:5] = sapply(df[3:5], as.numeric)

  df$`Total Employment` <- df$employed_full_time + df$employed_part_time

  df <- df[c("ioig", "industry_name", "Total Employment", "FTE Employment")]

  #Sometimes, for fun, the ABS like to change from # of people, to 1000's of people, without documentation.

  is_1000 <- any(grepl("'000", raw$`Employed Persons (a)`))

  if (isTRUE(is_1000)) {
    df <- df %>%
      dplyr::mutate(dplyr::across(c("Total Employment", "FTE Employment"), ~.x * 1000))
  } else {
    df <- df
  }

  dplyr::left_join(df, ioig_anzsic_div,  by = "ioig") %>%
    dplyr::group_by(.data$anzsic_division_code) %>%
    dplyr::summarise(dplyr::across(c("Total Employment", "FTE Employment"), sum), .groups = "drop") %>%
    tidyr::pivot_longer(cols = -"anzsic_division_code",
                        names_to = "from_anzsic") %>%
    tidyr::pivot_wider(names_from = "anzsic_division_code",
                       values_from = "value") %>%
    dplyr::arrange(factor(.data$from_anzsic, levels = c("FTE Employment", "Total Employment"))) %>%
    dplyr::mutate(`Total Industry Uses` = rowSums(dplyr::across(c(A:S))),
                  `Total Supply` = .data$`Total Industry Uses`)
}








