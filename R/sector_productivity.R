get_regional_sector_productivity <- function(data, region) {


  regional_production <- get_regional_employment(region, data$Year) %>%
    fte_employment(national_ratios = data$`FTE Ratios`) %>%
    dplyr::select(industry, employment) %>%
    dplyr::left_join(anzsic_swap, by = c("industry" = "name")) %>%
    dplyr::left_join(data$`Industry Productivity`, by = c("letter" = "industry")) %>%
    dplyr::mutate(`Regional Production` = productivity * employment) %>%
    dplyr::pull(`Regional Production`)

  names(regional_production) <- LETTERS[1:19]


  return(regional_production)
}
