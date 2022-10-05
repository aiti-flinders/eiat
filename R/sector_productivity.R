sector_productivity <- function(region, year) {

  industry_productivity <- create_19_sector() %>%
    tidyr::pivot_longer(cols = 2:length(.),
                 names_to = "anzsic_division_code",
                 values_to = "flow") %>%
    dplyr::filter(from_anzsic %in% c("Australian Production", "FTE Employment")) %>%
    tidyr::pivot_wider(names_from = from_anzsic, values_from = flow) %>%
    dplyr::mutate(productivity = `Australian Production`/`FTE Employment`)


  regional_production <- get_regional_employment(region, year) %>%
    fte_employment() %>%
    dplyr::select(anzsic_division_code, adjust_fte) %>%
    dplyr::left_join(industry_productivity, by = "anzsic_division_code") %>%
    dplyr::mutate(`Regional Production` = productivity * adjust_fte) %>%
    dplyr::pull(`Regional Production`)

  names(regional_production) <- LETTERS[1:19]

  return(regional_production)
}
