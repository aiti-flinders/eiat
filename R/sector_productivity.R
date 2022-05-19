sector_productivity <- function(region) {

  industry_productivity <- industry_industry_flows_19 %>%
    tidyr::pivot_longer(cols = 2:length(.),
                 names_to = "anzsic_division_code",
                 values_to = "flow") %>%
    dplyr::filter(from_anzsic %in% c("Australian Production", "Employed full-time")) %>%
    tidyr::pivot_wider(names_from = from_anzsic, values_from = flow) %>%
    dplyr::mutate(productivity = `Australian Production`/`Employed full-time`)

  overall_productivity <- industry_industry_flows_19 %>%
    tidyr::pivot_longer(cols = 2:length(.),
                 names_to = "anzsic_division_code",
                 values_to = "flow") %>%
    dplyr::filter(from_anzsic %in% c("P1", "P2", "P3", "P4", "Australian Production"),
           !anzsic_division_code %in% LETTERS)

  #get_region_employment <- function()

  regional_production <- get_regional_employment(region) %>%
    fte_employment() %>%
    dplyr::select(anzsic_division_code, adjust_fte) %>%
    dplyr::left_join(industry_productivity, by = "anzsic_division_code") %>%
    dplyr::mutate(`Regional Production` = productivity * adjust_fte) %>%
    dplyr::pull(`Regional Production`)

  names(regional_production) <- LETTERS[1:19]

  return(regional_production)
}
