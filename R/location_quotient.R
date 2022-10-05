
lq <- function(region, year) {

    data <- work[work$year == year, c("industry", "lga_pow", "employment", "year")] %>%
      dplyr::rename(lga = lga_pow) %>%
      adjust_employment() %>%
      dplyr::select(industry, lga, employment = adjust_jobs) %>%
      fte_employment()

    row_names <- unique(data$lga)
    col_names <- unique(data$industry)

    data <- tidyr::pivot_wider(data,
                               id_cols = lga,
                               names_from = industry,
                               values_from = employment)

    data_array <- as.matrix(data[2:length(data)])

    lq <- t(t(data_array/rowSums(data_array, na.rm = TRUE)) / (colSums(data_array, na.rm = TRUE)/sum(data_array, na.rm = TRUE)))

    lq %>%
      tibble::as_tibble() %>%
      dplyr::mutate(lga = row_names,
                    year = {{year}}) %>%
      tidyr::pivot_longer(cols = -c(lga, year),
                          names_to = "industry",
                          values_to = "lq") %>%
      dplyr::filter(lga == region)


    }
