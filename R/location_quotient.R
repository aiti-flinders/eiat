
lq <- function(region, year, path = NULL) {

    region_data <- work[work$year == year, c("industry", "lga_pow", "employment", "year")] %>%
      dplyr::rename(lga = lga_pow) %>%
      adjust_employment() %>%
      dplyr::select(industry, lga, employment = adjust_jobs) %>%
      fte_employment(path) %>%
      dplyr::filter(lga == region) %>%
      dplyr::mutate(region_ratio = employment / sum(employment))

    national_data <- fte_industry_ratio(path)

    lq <- region_data %>%
      dplyr::left_join(national_data, by = "industry") %>%
      dplyr::mutate(lq = region_ratio / national_ratio,
                    lq = ifelse(lq < 1, lq, 1)) %>%
      dplyr::pull(lq)

   return(lq)


    }
