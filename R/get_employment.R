#' Local employment
#' @description Return the local employment in a specified local government area. Local employment refers to
#' those who live and work in the region.
#' Data can adjusted to remove the Inadequately described and Not stated employment. The adjustment process
#' assumes that employment in Inadequately described and Not stated has the same distribution of employment by
#' industry as the reported data. For example, if local employment in Inadequately described totals 20,
#' and local employment in Agriculture, Forestry and Fishing in the region is 5% of total local employment, then
#' 5% * 20 of the Inadequately described employment is proportioned to Agriculture, Forestry and Fishing.
#'
#' Data is derived from the Australian Bureau of Statistics Census - Employment by industry by place of work by usual
#' residence for 2011, 2016 and 2021.
#'
#'
#' @param region A Local Government Area (LGA). See: `get_available_regions()`
#' @param year The census year to draw the data from. Must be a census year (2011, 2016, 2021).
#' @param adjust logical. The default (TRUE) adjusts the data to remove Inadequately described and Not stated.
#'
#' @return A dataframe with the local employment of `region` by industry.
#' @export get_local_employment
#'
#' @examples
#' get_local_employment("Adelaide", 2021)
#' # In the 2016 Census, the Adelaide LGA is called 'Adelaide (C)' so the following will not work.
#' \dontrun{
#' get_local_employment("Adelaide" 2016)
#' }
get_local_employment <- function(region, year, adjust = TRUE) {

  if (!year %in% c(2011, 2016, 2021)) {
    stop("The `year` argument to `get_local_employment()` must be an ABS Census year (2011, 2016 or 2021).")
  }

  if (!region %in% get_available_regions()$lga) {
    stop(glue::glue("Unable to find the region: {region} for {year}. Local Government Area names change between
                    Census years. Try `get_available_regions({year})`"))
  }


  employment <- eiat::live_and_work %>%
    dplyr::filter(dplyr::if_all(c("lga_pow", "lga_ur"), ~ .x == region),
                  .data$year == {{year}}) %>%
    dplyr::mutate(lga = region, .before = 1) %>%
    dplyr::select(-c("lga_pow", "lga_ur"))

  if (adjust) {
    employment %>%
      adjust_employment() %>%
      dplyr::select("lga", "industry", employment = "adjust_jobs")
  } else {
    employment
  }
}

#' Regional employment
#' @description Return the regional employment in a specified local government area. Regional employment refers to
#' those who work in the region but live outside the region.
#' Data can adjusted to remove the Inadequately described and Not stated employment. The adjustment process
#' assumes that employment in Inadequately described and Not stated has the same distribution of employment by
#' industry as the reported data. For example, if regional employment in Inadequately described totals 20,
#' and regional employment in Agriculture, Forestry and Fishing in the region is 5% of total regional employment, then
#' 5% * 20 of the Inadequately described employment is proportioned to Agriculture, Forestry and Fishing.
#'
#' Data is derived from the Australian Bureau of Statistics Census - Employment by industry by place of work
#' for 2011, 2016 and 2021.
#'
#'
#'
#' @param region A Local Government Area (LGA). See: `get_available_regions()`
#' @param year The census year to draw the data from. Must be a census year (2011, 2016, 2021).
#' @param adjust logical. The default (TRUE) adjusts the data to remove Inadequately described and Not stated.
#'
#' @return A dataframe with the regional employment of `region` by industry.
#' @export get_regional_employment
#'
#' @examples
#' get_regional_employment("Adelaide", 2021)
#' # In the 2016 Census, the Adelaide LGA is called 'Adelaide (C)' so the following will not work.
#' \dontrun{
#' get_regional_employment("Adelaide" 2016)
#' }
get_regional_employment <- function(region, year, adjust = TRUE) {

  if (!year %in% c(2011, 2016, 2021)) {
    stop("The `year` argument to `get_regional_employment()` must be an ABS Census year (2011, 2016 or 2021).")
  }

  if (!region %in% get_available_regions()$lga) {
    stop(glue::glue(
    "Unable to find the region: {region} for {year}.
    Local Government Area names change between Census years.
    Try `get_available_regions({year})`")
    )
  }


  employment <- eiat::work %>%
    dplyr::filter(.data$lga_pow == region,
                  .data$year == {{year}}) %>%
    dplyr::rename(lga = "lga_pow")

  if (adjust) {
    employment %>%
      adjust_employment() %>%
      dplyr::select("lga", "industry", employment = "adjust_jobs")
  }
}
