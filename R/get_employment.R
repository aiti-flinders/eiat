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
get_local_employment <- function(region, year, adjust = TRUE) {

  # Check that the region exists in the available_regions

  if (!region %in% get_available_regions(year = {{year}})) {
    dym <- get_available_regions()[stringdist::amatch(region, get_available_regions(year = {{year}}), method = "jw", maxDist = 0.4)]

    warning(paste0(region, " not found in local employment data. Did you mean: '", dym, "' ?"))
  }


  employment <- live_and_work %>%
    dplyr::filter(dplyr::if_all(c(lga_pow, lga_ur), ~ .x == region),
                  year == {{year}}) %>%
    dplyr::mutate(lga = region, .before = 1) %>%
    dplyr::select(-c(lga_pow, lga_ur))

  if (adjust) {
    employment %>%
      adjust_employment() %>%
      dplyr::select(lga, industry, employment = adjust_jobs)
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
get_regional_employment <- function(region, year, adjust = TRUE) {
  employment <- work %>%
    dplyr::filter(lga_pow == region,
                  year == {{year}}) %>%
    dplyr::rename(lga = lga_pow)

  if (adjust) {
    employment %>%
      adjust_employment() %>%
      dplyr::select(lga, industry, employment = adjust_jobs)
  }
}
