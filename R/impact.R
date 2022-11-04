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
#' @importFrom magrittr %>%
impact_analysis <- function(region, impacts) {

  rtt <- lq_models[[region]]

  basic_price <- rtt[c(1:19,21,22), c(1:19, 21:22)]
  total <- rtt[c("Australian Production"), c(1:19, 21:22)]

  A <- basic_price[1:19, 1:19] %*% diag(ifelse(is.infinite(1/total[1:19]), 0, 1/total[1:19]))

  inv_I_A <- solve(diag(1, 19, 19) - A)
  inv_I_A <- rbind(inv_I_A, colSums(inv_I_A))

  A_closed <- basic_price[1:20, 1:20] %*% diag(ifelse(is.infinite(1/total[1:20]), 0, 1/total[1:20]))

  inv_I_A_bar <- solve(diag(1, 20, 20) - A_closed)
  inv_I_A_bar <- rbind(inv_I_A_bar, c(colSums(inv_I_A_bar[, 1:19]), 0))

  sector_impacts <- impacts


  #Output
  output_m <- inv_I_A_bar[1:19, 1:19] %*% sector_impacts
  total_output <- output_m
  consumption_induced_output <- (inv_I_A_bar[1:19, 1:19] - inv_I_A[1:19, 1:19]) %*% sector_impacts
  production_induced_output <- total_output - sector_impacts - consumption_induced_output



  output_df <- data.frame(
    fix.empty.names = FALSE,
    row.names = NULL,
    "Sector" = rownames(impacts),
    "Direct Output" = sector_impacts[, 1:ncol(impacts)],
    "Production Induced Output" = production_induced_output[, 1:ncol(impacts)],
    "Consumption Induced Output" = consumption_induced_output[, 1:ncol(impacts)],
    "Flow-on Output" = production_induced_output[, 1:ncol(impacts)] + consumption_induced_output[, 1:ncol(impacts)],
    "Total Output" = total_output[, 1:ncol(impacts)]
  )



  #FTE, HH Income, Value Added
  fte_hh_v <- rtt[c("Wages and Salaries - Local",
                    "Wages and Salaries - Other",
                    "Gross operating surplus and mixed income",
                    "Taxes less subsidies on products and production",
                    "Local Employment",
                    "Other Employment",
                    "Total Employment"),1:19] %*% diag(ifelse(is.infinite(1/rtt["Australian Production", 1:19]), 0, 1/rtt["Australian Production", 1:19]), names = T)

  grp <- colSums(fte_hh_v[1:4, ])

  fte_hh_v <- rbind(fte_hh_v, grp)

  #GRP
  initial_grp <- grp * sector_impacts
  consumption_induced_grp <- (inv_I_A_bar[1:19, 1:19]*grp - inv_I_A[1:19, 1:19]*grp) %*% sector_impacts
  total_grp <- (inv_I_A_bar[1:19, 1:19]*grp) %*% sector_impacts
  production_induced_grp <- total_grp - initial_grp - consumption_induced_grp

  grp_df <- data.frame(
    row.names = NULL,
    "Sector" = rownames(impacts),
    "Direct GRP" = initial_grp[, 1:ncol(impacts)],
    "Production Induced GRP" = production_induced_grp[, 1:ncol(impacts)],
    "Consumption Induced GRP" = consumption_induced_grp[, 1:ncol(impacts)],
    "Flow-on GRP" = production_induced_grp[, 1:ncol(impacts)] + consumption_induced_grp[, 1:ncol(impacts)],
    "Total GRP" = total_grp[, 1:ncol(impacts)]
  )

  #Employment
  emp <- fte_hh_v["Total Employment", 1:19]
  initial_employment <- emp * sector_impacts
  consumption_induced_employment <- (inv_I_A_bar[1:19, 1:19] * emp - inv_I_A[1:19, 1:19] * emp) %*% sector_impacts
  total_employment <- (inv_I_A_bar[1:19, 1:19]*emp) %*% sector_impacts
  production_induced_employment <- total_employment - initial_employment - consumption_induced_employment

  emp_df <- data.frame(
    row.names = NULL,
    "Sector" = rownames(impacts),
    "Direct Employment" = initial_employment[, 1:ncol(impacts)],
    "Production Induced Employment" = production_induced_employment[, 1:ncol(impacts)],
    "Consumption Induced Employment" = consumption_induced_employment[, 1:ncol(impacts)],
    "Flow-on Employment" = production_induced_employment[, 1:ncol(impacts)] + consumption_induced_employment[, 1:ncol(impacts)],
    "Total Employment" = total_employment[, 1:ncol(impacts)]
  )



  out <- list("output" = output_df,
              "grp" = grp_df,
              "emp" = emp_df)

  out <- lapply(out, reshape_output, impacts = impacts)

  out$region <- region

  return(out)

}

reshape_output <- function(data, impacts) {

  if (dim(impacts)[2] > 1) {
    data  %>%
      tidyr::pivot_longer(cols = -Sector,
                          names_to = c("type", "year"),
                          names_pattern = "^(.*[\\.])(\\d{4})") %>%
      dplyr::mutate(type = trimws(gsub("\\.", " ", type)))
  } else {
    data %>%
      tidyr::pivot_longer(cols = -Sector,
                          names_to = "type",
                          values_to = "value") %>%
      dplyr::mutate(year = 2022 + (1 - ncol(impacts)),
                    type = trimws(gsub("\\.", " ", type)))
  }

}
