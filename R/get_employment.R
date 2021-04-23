#' Return the number of people living and working in a specified local government area.
#' Data can adjusted to remove not specified, not applicable, nfd industries.
#'
#'
#' @param region which Local Government Area (LGA) to return.
#' @param adjust logical. The default (TRUE) adjusts the data to remove inadequately described and not stated.
#'
#' @return
#' @export
#'
#' @examples
get_local_employment <- function(region, adjust = TRUE) {

  employment <- local_employment %>%
    dplyr::filter(across(c(place_of_work, usual_residence), ~.x == region)) %>%
    dplyr::mutate(lga = region, .before = 1) %>%
    dplyr::select(-c(place_of_work, usual_residence))

  if (adjust) {
   employment %>%
      adjust_employment() %>%
      select(lga, anzsic_division_code, adjust_jobs)
  } else {
    employment
  }

}

#' Return the number of people working in a specified local government area, by industry.
#' Data can adjusted to remove not specified, not applicable, nfd industries.
#'
#'
#' @param region which Local Government Area (LGA) to return.
#' @param adjust logical. The default (TRUE) adjusts the data to remove inadequately described and not stated.
#'
#' @return
#' @export
#'
#' @examples
get_regional_employment <- function(region, adjust = TRUE) {
  employment <- regional_employment %>%
    dplyr::filter(lga == region)

  if (adjust) {
    employment %>%
      adjust_employment() %>%
      select(lga, anzsic_division_code, adjust_jobs)
  }

}
