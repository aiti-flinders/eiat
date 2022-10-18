get_data <- function(year, path = NULL) {


    m <- create_19_sector()

    fte_ratios <- m %>%
    dplyr::filter(from_anzsic %in% c("FTE Employment", "Total Employment")) %>%
      tidyr::pivot_longer(cols = A:S,
                          names_to = "anzsic_division_code",
                          values_to = "value") %>%
      dplyr::select(anzsic_division_code, from_anzsic, value) %>%
      tidyr::pivot_wider(names_from = from_anzsic,
                         values_from = value) %>%
      dplyr::mutate(fte = `FTE Employment`/`Total Employment`) %>%
      dplyr::left_join(anzsic_swap, by = c("anzsic_division_code" = "letter"))

    fte_industry_ratio <- m %>%
      dplyr::filter(from_anzsic %in% c("FTE Employment")) %>%
      tidyr::pivot_longer(cols = A:S,
                          names_to = "industry",
                          values_to = "employment") %>%
      dplyr::select(industry, employment) %>%
      dplyr::left_join(anzsic_swap, by = c("industry" = "letter")) %>%
      dplyr::select(industry = name, employment) %>%
      dplyr::mutate(national_ratio = employment / sum(employment)) %>%
      dplyr::select(-employment)



    industry_productivity <- m %>%
      tidyr::pivot_longer(cols = 2:length(.),
                          names_to = "industry",
                          values_to = "flow") %>%
      dplyr::filter(from_anzsic %in% c("Australian Production", "FTE Employment")) %>%
      tidyr::pivot_wider(names_from = from_anzsic, values_from = flow) %>%
      dplyr::filter(industry %in% LETTERS[1:19]) %>%
      dplyr::mutate(productivity = `Australian Production`/`FTE Employment`)

    lqs <-  work[work$year == {{year}}, c("industry", "lga_pow", "employment", "year")] %>%
      dplyr::rename(lga = lga_pow) %>%
      adjust_employment() %>%
      dplyr::select(industry, lga, employment = adjust_jobs) %>%
      dplyr::left_join(fte_ratios, by = c("industry" = "name")) %>%
      dplyr::mutate(employment = employment * fte) %>%
      dplyr::select(-c(anzsic_division_code, `FTE Employment`, `Total Employment`, fte)) %>%
      dplyr::group_by(lga) %>%
      dplyr::mutate(region_ratio = ifelse(is.nan(employment/sum(employment)), 0, employment / sum(employment))) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(fte_industry_ratio, by = "industry") %>%
      dplyr::mutate(lq = region_ratio / national_ratio,
                    lq = ifelse(lq < 1, lq, 1))



  out <- list("Australia 19" = m,
              "Industry Productivity" = industry_productivity,
              "FTE Ratios" = fte_ratios,
              "Location Quotients" = lqs,
              "Year" = year)
}
