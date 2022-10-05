add_non_anzsic_rows <- function(data) {
  data %>%
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
                  adjust_jobs = adjust_inadequate + (adjust_inadequate/sum(adjust_inadequate)) * `Not stated`) %>%
    dplyr::filter(industry != "Not applicable") %>%
    dplyr::ungroup()


}

fte_employment <- function(data) {
  ratio <- create_19_sector() %>%
    dplyr::filter(from_anzsic %in% c("FTE Employment", "Total Employment")) %>%
    tidyr::pivot_longer(cols = A:S,
                        names_to = "anzsic_division_code",
                        values_to = "value") %>%
    dplyr::select(anzsic_division_code, from_anzsic, value) %>%
    tidyr::pivot_wider(names_from = from_anzsic,
                       values_from = value) %>%
    dplyr::mutate(fte = `FTE Employment`/`Total Employment`) %>%
    dplyr::left_join(anzsic_swap, by = c("anzsic_division_code" = "letter"))

  data %>%
    dplyr::left_join(ratio, by = c("industry" = "name")) %>%
    dplyr::mutate(employment = employment * fte) %>%
    dplyr::select(-c(anzsic_division_code, `FTE Employment`, `Total Employment`, fte))
}

get_available_regions <- function() {
  local_employment %>%
    dplyr::distinct(usual_residence) %>%
    dplyr::rename(region_lga = usual_residence) %>%
    dplyr::pull(region_lga)
}

anzsic_letter_to_number <- function() {

}

ioig_names_number <- function(col_no) {
  names(io_cols[col_no])

}
