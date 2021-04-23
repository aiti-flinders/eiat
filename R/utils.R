employment_industry_fy <- function(financial_year) {

  fy_left <- strsplit(financial_year, split = "-")[[1]][1]
  fy_right <- as.character(as.numeric(fy_left)+1)

  date_left <- as.Date(paste(fy_left, "07-01", sep = "-"))
  date_right <- as.Date(paste(fy_right, "06-30", sep = "-"))



  aitidata::employment_by_industry %>%
    filter(between(date, date_left, date_right),
           gender == "Persons", age == "Total (age)",
           state == "Australia",
           indicator %in% c("Employed full-time", "Employed total"),
           industry != "Total (industry)") %>%
    left_join(abscorr::anzsic, by = c("industry" = "anzsic_division")) %>%
    distinct(indicator, anzsic_division_code, value) %>%
    group_by(indicator, anzsic_division_code) %>%
    summarise(value = mean(value)) %>%
    pivot_wider(names_from = anzsic_division_code) %>%
    ungroup() %>%
    rename(from_anzsic = indicator) %>%
    mutate(total_industry_uses = rowSums(across(c(2:20))))
}

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
    tidyr::pivot_longer(cols = 2:(length(.)-4),
                        names_to = "anzsic_division_code",
                        values_to = "jobs") %>%
    dplyr::mutate(adjust_inadequate = jobs + (jobs/sum(jobs)) * `Inadequately described`,
                  adjust_jobs = adjust_inadequate + (adjust_inadequate/sum(adjust_inadequate)) * `Not stated`)

}

fte_employment <- function(.data) {
  ratio <- industry_industry_flows_19 %>%
    dplyr::filter(from_anzsic %in% c("Employed full-time", "Employed total")) %>%
    tidyr::pivot_longer(cols = A:S,
                        names_to = "anzsic_division_code",
                        values_to = "value") %>%
    dplyr::select(anzsic_division_code, from_anzsic, value) %>%
    tidyr::pivot_wider(names_from = from_anzsic,
                       values_from = value) %>%
    mutate(fte = `Employed full-time`/`Employed total`)

  .data %>%
    dplyr::left_join(ratio, by = "anzsic_division_code") %>%
    mutate(adjust_fte = adjust_jobs * fte) %>%
    select(lga, anzsic_division_code, adjust_fte)
}


