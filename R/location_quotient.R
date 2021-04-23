#' Calculate the Location Quotients for a region
#'
#' @param region
#'
#' @return
#' @export
#'
#' @examples
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr %>%
lq <- function(region) {

  lga_employment <- get_regional_employment(region) %>%
    fte_employment()

  lga_employment_local <- get_local_employment(region) %>%
    fte_employment()

  regional_lq <- lga_employment %>%
    mutate(lga_lq = adjust_fte/sum(adjust_fte))

  #Temporary while not having all the data - otherwise can calculate LQ similar to RCA
  ratio <- industry_industry_flows_19 %>%
    dplyr::filter(from_anzsic %in% c("Employed full-time", "Employed total")) %>%
    tidyr::pivot_longer(cols = A:S,
                        names_to = "anzsic_division_code",
                        values_to = "value") %>%
    dplyr::select(anzsic_division_code, from_anzsic, value) %>%
    tidyr::pivot_wider(names_from = from_anzsic,
                       values_from = value) %>%
    dplyr::mutate(nat_lq = `Employed full-time`/sum(`Employed full-time`))

  national_lq <- ratio %>%
    dplyr::mutate(nat_lq = `Employed full-time`/sum(`Employed full-time`))

  lq <- left_join(regional_lq, national_lq, by = "anzsic_division_code") %>%
    dplyr::select(anzsic_division_code, lga_lq, nat_lq) %>%
    dplyr::mutate(lq = lga_lq/nat_lq,
           lq = ifelse(lq < 1, lq, 1)) %>%
    dplyr::pull(lq)

  return(lq)
}





