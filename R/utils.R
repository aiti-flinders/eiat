add_non_anzsic_rows <- function(.data) {
  .data %>%
    tibble::add_row(anzsic_division_code = "Total Intermediate Uses", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "P1", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "P2", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "P3", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "P4", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "Australian Production", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "Employed full-time", lq = 1) %>%
    tibble::add_row(anzsic_division_code = "Employed total", lq = 1)
}


adjust_employment <- function(.data) {
  .data %>%
    tidyr::pivot_wider(names_from = .data$industry,
                       values_from = .data$employment) %>%
    tidyr::pivot_longer(cols = -c(.data$lga, .data$year, .data$`Inadequately described`, .data$`Not stated`),
                        names_to = "industry",
                        values_to = "employment") %>%
    dplyr::group_by(.data$lga) %>%
    dplyr::mutate(adjust_inadequate = .data$employment + (.data$employment/sum(.data$employment)) * .data$`Inadequately described`,
                  adjust_jobs = .data$adjust_inadequate + (.data$adjust_inadequate/sum(.data$adjust_inadequate)) * .data$`Not stated`,
                  dplyr::across(c(.data$adjust_inadequate, .data$adjust_jobs), ~ifelse(is.nan(.x), 0, .x))) %>%
    dplyr::filter(.data$industry != "Not applicable") %>%
    dplyr::ungroup()



}

fte_industry_ratio <- function(path = NULL) {
  ratio <- create_19_sector(path) %>%
    dplyr::filter(.data$from_anzsic %in% c("FTE Employment")) %>%
    tidyr::pivot_longer(cols = "A:S",
                        names_to = "industry",
                        values_to = "employment") %>%
    dplyr::select("industry",
                  "employment") %>%
    dplyr::left_join(anzsic_swap, by = c("industry" = "letter")) %>%
    dplyr::select(industry = "name",
                  "employment") %>%
    dplyr::mutate(national_ratio = .data$employment / sum(.data$employment)) %>%
    dplyr::select(-"employment")

  return(ratio)
}

fte_employment <- function(regional_employment, national_ratios) {
  ratio <- national_ratios

  regional_employment %>%
    dplyr::left_join(ratio, by = c("industry" = "industry")) %>%
    dplyr::mutate(employment = .data$employment * .data$fte) %>%
    dplyr::select(-c("FTE Employment",
                     "Total Employment",
                     "fte"))
}

get_available_regions <- function() {

  regions

}

get_regional_sector_productivity <- function(data, region) {


  regional_production <- get_regional_employment(region, data$Year) %>%
    fte_employment(national_ratios = data$`FTE Ratios`) %>%
    dplyr::select("industry", "employment") %>%
    dplyr::left_join(data$`Industry Productivity`, by = c("industry")) %>%
    dplyr::mutate(`Regional Production` = .data$productivity * .data$employment) %>%
    dplyr::pull(.data$`Regional Production`)

  names(regional_production) <- anzsic_swap$name


  return(regional_production)
}


ioig_names_number <- function(col_no) {
  names(io_cols[col_no])

}

#' @importFrom utils packageVersion
eiat_version <- function() {
  utils::packageVersion("eiat")
}
