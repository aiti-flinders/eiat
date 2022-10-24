add_non_anzsic_rows <- function(.data) {
  .data %>%
    add_row(anzsic_division_code = "Total Intermediate Uses", lq = 1) %>%
    add_row(anzsic_division_code = "P1", lq = 1) %>%
    add_row(anzsic_division_code = "P2", lq = 1) %>%
    add_row(anzsic_division_code = "P3", lq = 1) %>%
    add_row(anzsic_division_code = "P4", lq = 1) %>%
    add_row(anzsic_division_code = "Australian Production", lq = 1) %>%
    add_row(anzsic_division_code = "Employed full-time", lq = 1) %>%
    add_row(anzsic_division_code = "Employed total", lq = 1)
}


adjust_employment <- function(.data) {
  .data %>%
    tidyr::pivot_wider(names_from = industry,
                       values_from = employment) %>%
    tidyr::pivot_longer(cols = -c(lga, year, `Inadequately described`, `Not stated`),
                        names_to = "industry",
                        values_to = "employment") %>%
    dplyr::group_by(lga) %>%
    dplyr::mutate(adjust_inadequate = employment + (employment/sum(employment)) * `Inadequately described`,
                  adjust_jobs = adjust_inadequate + (adjust_inadequate/sum(adjust_inadequate)) * `Not stated`,
                  dplyr::across(c(adjust_inadequate, adjust_jobs), ~ifelse(is.nan(.x), 0, .x))) %>%
    dplyr::filter(industry != "Not applicable") %>%
    dplyr::ungroup()



}

fte_industry_ratio <- function(path = NULL) {
  ratio <- create_19_sector(path) %>%
    dplyr::filter(from_anzsic %in% c("FTE Employment")) %>%
    tidyr::pivot_longer(cols = A:S,
                        names_to = "industry",
                        values_to = "employment") %>%
    dplyr::select(industry, employment) %>%
    dplyr::left_join(anzsic_swap, by = c("industry" = "letter")) %>%
    dplyr::select(industry = name, employment) %>%
    dplyr::mutate(national_ratio = employment / sum(employment)) %>%
    dplyr::select(-employment)

  return(ratio)
}

fte_employment <- function(regional_employment, national_ratios) {
  ratio <- national_ratios

  regional_employment %>%
    dplyr::left_join(ratio, by = c("industry" = "industry")) %>%
    dplyr::mutate(employment = employment * fte) %>%
    dplyr::select(-c(`FTE Employment`, `Total Employment`, fte))
}

get_available_regions <- function(year) {

  if ({{year}} == 2011) {states <- lga_2011}
  else if ({{year}} == 2016) {states <- lga_2016}
  else if ({{year}} == 2021) {states <- lga_2021}

  work %>%
    dplyr::filter(year == {{year}}) %>%
    dplyr::distinct(lga_pow) %>%
    dplyr::rename(lga = lga_pow) %>%
    dplyr::left_join(states, by = c("lga"))
}

anzsic_letter_to_number <- function() {

}

ioig_names_number <- function(col_no) {
  names(io_cols[col_no])

}

eiat_version <- function() {
  packageVersion("eiat")
}
