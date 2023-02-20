#' Generate Data For Regional Input-Output Table Creation
#'
#' @description `get_data` creates a list of data frames containing all the data necessary to build a Regional Input-Output
#' Table.
#' \loadmathjax{}
#'
#' @details
#' Regional Input-Output Tables are created using the Location Quotient method and are derived from the Australian 19 Sector
#' Input-Output Table. This function provides:
#' \itemize{
#' \item{Australian 19 Sector Input-Output Table}{}
#' \item{Industry Productivity}{}
#' \item{FTE Ratios}{}
#' \item{Regional Location Quotients}{}
#' }
#'
#' ## Australian 19 Sector Input-Output Table
#' The 19 Sector Input-Output Table for Australia is derived from the Australian National Accounts: Input-Output Tables.
#' The Input-Output Table provided by the Australian Bureau of Statistics is more detailed - describing industry-industry flows
#' for 114 sectors. As Regional Input-Output Tables are *estimated* using the Location Quotient method, an aggregated 19
#' Sector Input-Output Table is used which is a trade-off between availability and reliability of regional data, and
#' specificity of the regional model.
#'
#' ## Industry Productivity
#' Industry productivity measures the average production per FTE employment across Australia.
#'
#' ## FTE Ratios
#' The ratio of FTE Employment to Total Employment across all industries in Australia.
#'
#' ## Regional Location Quotients
#' Location Quotients are used to determine if an industry in a region is "as significant" as the industry in the country.
#' That is, whether or not a region capable of supplying to local industries at the same proportion as the country as a whole.
#' If not, the region supplies proportionally less to local industries. Regional employment is used to determine whether or not
#' an industry in a region is "significant" where significance is defined as:
#'
#' \mjdeqn{
#' LQ_{i,r} = \frac{E_{i, r}}{\sum_i{E_{i, r}}} / \frac{\sum_{r}{E_{i, r}}}{\sum_{r,i}{E_{i,r}}}
#' }{}
#' Where \mjeqn{E_{i,r}}{} is the FTE Employment in region r, and industry i.
#'
#' An industry in a region is said to be significant if \mjeqn{LQ_{i,r} >=1}{}. In these instances, the regional coefficient
#' for a supplying industry is assumed to be the same as the national coefficient for the supplying industry. Otherwise, the
#' regional coefficient is estimated as \mjeqn{LQ_{i,r}\times a_{i,r}}{} where \mjeqn{a_{i,r}}{} is the national coefficient.
#'
#'
#'
#' @param year
#'
#' @return
#' @export
#' @import mathjaxr
#'
#' @examples
#' get_data(2021)
get_data <- function(year) {


    m <- create_19_sector()

    fte_ratios <- m %>%
    dplyr::filter(`Industry Sector` %in% c("FTE Employment", "Total Employment")) %>%
      tidyr::pivot_longer(cols = 2:20,
                          names_to = "industry",
                          values_to = "value") %>%
      dplyr::select(industry, `Industry Sector`, value) %>%
      tidyr::pivot_wider(names_from = `Industry Sector`,
                         values_from = value) %>%
      dplyr::mutate(fte = `FTE Employment`/`Total Employment`)

    fte_industry_ratio <- m %>%
      dplyr::filter(`Industry Sector`  %in% c("FTE Employment")) %>%
      tidyr::pivot_longer(cols = 2:20,
                          names_to = "industry",
                          values_to = "employment") %>%
      dplyr::select(industry, employment) %>%
      dplyr::mutate(national_ratio = employment / sum(employment)) %>%
      dplyr::select(-employment)



    industry_productivity <- m %>%
      tidyr::pivot_longer(cols = 2:length(.),
                          names_to = "industry",
                          values_to = "flow") %>%
      dplyr::filter(`Industry Sector` %in% c("Australian Production", "FTE Employment")) %>%
      tidyr::pivot_wider(names_from = `Industry Sector`, values_from = flow) %>%
      dplyr::filter(industry %in% anzsic_swap$name) %>%
      dplyr::mutate(productivity = `Australian Production`/`FTE Employment`)

    lqs <-  work[work$year == {{year}}, c("industry", "lga_pow", "employment", "year")] %>%
      dplyr::rename(lga = lga_pow) %>%
      adjust_employment() %>%
      dplyr::select(industry, lga, employment = adjust_jobs) %>%
      dplyr::left_join(fte_ratios, by = "industry") %>%
      dplyr::mutate(employment = employment * fte) %>%
      dplyr::select(-c(`FTE Employment`, `Total Employment`, fte)) %>%
      dplyr::group_by(lga) %>%
      dplyr::mutate(region_ratio = ifelse(is.nan(employment/sum(employment)), 0, employment / sum(employment))) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(fte_industry_ratio, by = "industry") %>%
      dplyr::mutate(lq = region_ratio / national_ratio,
                    lq = ifelse(lq < 1, lq, 1))



  out <- list("Australia 19" = m,
              "Industry Productivity" = industry_productivity,
              "FTE Ratios" = fte_ratios,
              "Location Quotients" = lqs,
              "Year" = year)
}
