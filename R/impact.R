#' Title
#'
#' @param region
#' @param year
#' @param path
#' @param impacts
#'
#' @return
#' @export
#'
#' @examples
impact_analysis <- function(region, year, path = NULL, impacts) {

  rtt <- lq_models[[region]]

  basic_price <- rtt[c(1:19,21,22), c(1:19, 21:22)]

  total <- rtt[c("Australian Production"), c(1:19, 21:22)]

  A <- basic_price[1:19, 1:19] %*% diag(1/total[1:19])

  inv_I_A <- solve(diag(1, 19, 19) - A)
  inv_I_A <- rbind(inv_I_A, colSums(inv_I_A[, 1:19]))

  A_closed <- basic_price[1:20, 1:20] %*% diag(1/total[1:20])

  inv_I_A_bar <- solve(diag(1, 20, 20) - A_closed)
  inv_I_A_bar <- rbind(inv_I_A_bar, c(colSums(inv_I_A_bar[, 1:19]), 0))
  consumption_induced <- inv_I_A_bar[21,] - c(inv_I_A[20,], 0)

  sector_impacts <- impacts


  #Output
  output_m <- inv_I_A_bar[1:19, 1:19] * sector_impacts
  total_output <- rowSums(output_m)
  consumption_induced_output <- rowSums((inv_I_A_bar[1:19, 1:19] - inv_I_A[1:19, 1:19])*sector_impacts)
  production_induced_output <- total_output - sector_impacts - consumption_induced_output

  output_df <- data.frame(
    "Sector" = LETTERS[1:19],
    "Direct Output" = sector_impacts,
    "Production Induced Output" = production_induced_output,
    "Consumption Induced Output" = consumption_induced_output,
    "Total Output" = total_output
  )




  #FTE, HH Income, Value Added
  fte_hh_v <- rtt[c("Wages and Salaries - Local",
                    "Wages and Salaries - Other",
                    "Gross operating surplus and mixed income",
                    "Taxes less subsidies on products and production",
                    "Local Employment",
                    "Other Employment",
                    "Total Employment"),1:19] %*% diag(1/rtt["Australian Production", 1:19], names = T)

  grp <- colSums(fte_hh_v[1:4, ])

  fte_hh_v <- rbind(fte_hh_v, grp)

  #GRP
  initial_grp <- grp * sector_impacts
  consumption_induced_grp <- sector_impacts * rowSums(inv_I_A_bar[1:19, 1:19]*grp - inv_I_A[1:19, 1:19]*grp)
  total_grp <- rowSums(inv_I_A_bar[1:19, 1:19]*sector_impacts*grp)
  production_induced_grp <- total_grp - initial_grp - consumption_induced_grp

  grp_df <- data.frame(
    "Sector" = LETTERS[1:19],
    "Direct GRP" = initial_grp,
    "Production Induced GRP" = production_induced_grp,
    "Consumption Induced GRP" = consumption_induced_grp,
    "Total GRP" = total_grp
  )



  #Employment
  emp <- fte_hh_v["Total Employment", 1:19]
  initial_employment <- emp * sector_impacts
  consumption_induced_employment <- sector_impacts * rowSums(inv_I_A_bar[1:19, 1:19] * emp - inv_I_A[1:19, 1:19] * emp)
  total_employment <- rowSums(inv_I_A_bar[1:19, 1:19]*sector_impacts*emp)
  production_induced_employment <- total_employment - initial_employment - consumption_induced_employment

  emp_df <- data.frame(
    "Sector" = LETTERS[1:19],
    "Direct Employment" = initial_employment,
    "Production Induced Employment" = production_induced_employment,
    "Consumption Induced Employment" = consumption_induced_employment,
    "Total Employment" = total_employment
  )

  dplyr::left_join(
    output_df,
    grp_df,
    by = "Sector"
  ) %>%
    dplyr::left_join(
      emp_df,
      by = "Sector"
    )

}
