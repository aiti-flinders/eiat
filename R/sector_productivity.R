get_regional_sector_productivity <- function(data, region) {


  regional_production <- get_regional_employment(region, data$Year) %>%
    fte_employment(national_ratios = data$`FTE Ratios`) %>%
    dplyr::select(industry, employment) %>%
    dplyr::left_join(data$`Industry Productivity`, by = c("industry")) %>%
    dplyr::mutate(`Regional Production` = productivity * employment) %>%
    dplyr::pull(`Regional Production`)

  names(regional_production) <- anzsic_swap$name


  return(regional_production)
}
