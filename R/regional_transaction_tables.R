#' Create basic transaction table for a region
#'
#' @param region
#'
#' @return
#' @export
#'
#' @examples
rtt_basic <- function(data, region, type = c("basic", "household")) {

  if (!is.list(data)) {
    stop("Need a list with: 19 sector IO, sector productivity, location quotients. You can get one from `get_data()`")
  }

  m <- data$`Australia 19` %>%
    dplyr::filter(!from_anzsic %in% c("Total Intermediate Use", "Total Employment")) %>%
    dplyr::select(-`Total Industry Uses`) %>%
    tibble::column_to_rownames("from_anzsic") %>%
    as.matrix()

  sector_prod <- get_regional_sector_productivity(data, region)

  location_quotient <- data$`Location Quotients` %>%
    dplyr::filter(lga == region) %>%
    dplyr::pull(lq)

  region_fte_employment <- get_regional_employment(region, data$Year) %>%
    fte_employment(national_ratios = data$`FTE Ratios`)


  # National Coefficients ---------------------------------------------------

  direct_coefficients <-  m %*% diag(1/m["Australian Production",], names = TRUE)
  colnames(direct_coefficients) <- colnames(m)


  # Regional Coefficients ---------------------------------------------------


  # Step 1: Industry rows and all columns (except total supply)

  regional_direct_coefficients <- diag(location_quotient) %*% direct_coefficients[1:19, !colnames(direct_coefficients) %in% c("Total Supply")]

  # Step 2: Add P1-P4 to Regional DC
  # Compensation of employees, Gross operating surplus & mixed income, Taxes less subsidies on products and production,
  # and Australian production are the same as in the national direct coefficients. We add imports now to adjust later.
  ix <- c("Compensation of employees",
          "Gross operating surplus & mixed income",
          "Taxes less subsidies on products and production",
          "Imports",
          "Australian Production")

  regional_direct_coefficients <- rbind(regional_direct_coefficients,
                                        direct_coefficients[ix, !colnames(direct_coefficients) %in% c("Total Supply")])

  # Step 3: Add Total Supply - its the same as the National DC

  regional_direct_coefficients <- cbind(regional_direct_coefficients, direct_coefficients[1:24, 25])

  # Step 4: Imports (P4) balance production (which has a DC of 1) and the sum of the transactions
  regional_direct_coefficients["Imports", -25] <- 1 - colSums(regional_direct_coefficients[1:22, -25])
  colnames(regional_direct_coefficients) <- colnames(m)



  # Regional Transactions ---------------------------------------------------

  # Industry sectors: National (total employment / fte employment) * Region FTE

  total_compensation <- regional_direct_coefficients[,1:19] %*% diag(sector_prod, names = TRUE)
  total_compensation <- sum(total_compensation["Compensation of employees", ])

  #HH "Productivity" and ""Other"" """Productivity"""  - this section is weird and needs better documentation
  hh_multiplier <- (m["Australian Production", "Households Final Consumption Expenditure"] / m["Compensation of employees", "Total Supply"]) * total_compensation
  other_multiplier <- m["Australian Production",
                        c("General Government Final Consumption Expenditure", "Gross Fixed Capital Formation", "Changes in Inventories")] / m["FTE Employment", "Total Supply"]

  other_multiplier <- other_multiplier*(region_fte_employment %>%
                                          dplyr::summarise(sum(employment)) %>%
                                          dplyr::pull())

  rtt_multiplier <- c(sector_prod, hh_multiplier, other_multiplier)



  rtt_basic <- regional_direct_coefficients[,1:23] %*% diag(rtt_multiplier, names = TRUE)
  rtt_basic <- cbind(rtt_basic, c(sector_prod, rep(NA, 5)))

  rtt_basic[c("Compensation of employees",
              "Gross operating surplus & mixed income",
              "Taxes less subsidies on products and production",
              "Imports"), 24] <- rowSums(rtt_basic[c("Compensation of employees",
                                                     "Gross operating surplus & mixed income",
                                                     "Taxes less subsidies on products and production",
                                                     "Imports"), ], na.rm = T)
  rtt_basic["Australian Production", 24] <- sum(rtt_basic[,24], na.rm = T)

  rtt_exports <- rtt_basic[, 24] - rowSums(rtt_basic[, 1:23])

  rtt_basic <- cbind(rtt_basic, rtt_exports)

  #Export column comes before total supply column
  rtt_basic[, c(24,25)] <- rtt_basic[, c(25,24)]

  #Name columns, name rows
  colnames(rtt_basic) <- colnames(direct_coefficients)
  rownames(rtt_basic) <- rownames(direct_coefficients[1:24,])

  local_employment <- get_local_employment(region, data$Year) %>%
    fte_employment(national_ratios = data$`FTE Ratios`) %>%
    dplyr::pull(employment)

  total_employment <- get_regional_employment(region, data$Year) %>%
    fte_employment(national_ratios = data$`FTE Ratios`) %>%
    dplyr::pull(employment)


  names(local_employment) <- LETTERS[1:19]
  names(total_employment) <- LETTERS[1:19]

  # Split HH Consumption into Local/Employed in region
  # Watch out for regions where there is no employment in an industry

  wages_and_salaries_local <- ifelse(is.nan(local_employment / total_employment), 0, local_employment / total_employment) * rtt_basic["Compensation of employees", 1:19]
  wages_and_salaries_other <- rtt_basic["Compensation of employees", 1:19] - wages_and_salaries_local

  rtt_hh <- rbind(rtt_basic[, -20],  c(wages_and_salaries_local, rep(0, 5)),  c(wages_and_salaries_other, rep(0, 5)))

  hh_multiplier_local <- (m["Australian Production", "Households Final Consumption Expenditure"] / m["Compensation of employees", "Total Supply"]) * sum(wages_and_salaries_local, na.rm = T)
  hh_consumption_employed_region <- c(regional_direct_coefficients[, "Households Final Consumption Expenditure"] * hh_multiplier_local, rep(0, 2))
  hh_consumption_local <- c(rtt_basic[, "Households Final Consumption Expenditure"], rep(0, 2)) - hh_consumption_employed_region

  # Household Split

  rtt_hh <- cbind(rtt_hh, hh_consumption_employed_region, hh_consumption_local)

  intermediate_inputs <- colSums(rtt_hh[1:19, 1:26])

  rtt_hh <- rbind(rtt_hh, intermediate_inputs)

  intermediate_demand <- rowSums(rtt_hh[1:27 ,1:19])

  rtt_hh <- cbind(rtt_hh, intermediate_demand)


  rtt_hh <- rtt_hh[c(1:19, 27, 25, 26, 21:24), c(1:19, 27, 25, 26, 20:24)]

  rownames(rtt_hh) <- c(LETTERS[1:19],
                        "Intermediate Inputs",
                        "Wages and Salaries - Local",
                        "Wages and Salaries - Other",
                        "Gross operating surplus and mixed income",
                        "Taxes less subsidies on products and production",
                        "Imports",
                        "Australian Production")

  rtt_hh[c("Wages and Salaries - Local", "Wages and Salaries - Other"), "Total Supply"] <- rowSums(rtt_hh[c("Wages and Salaries - Local", "Wages and Salaries - Other"), 20:26])

  #this is looking pretty good. Final check is for negative exports


  # If exports are negative, apportion total supply across industries
  #1. Which industries have negative exports:
  ix <- which(rtt_hh[1:19, "Exports of Goods and Services"] < 0)

  z <-  rowSums(rtt_hh[ix, c(1:19, 21:25), drop = FALSE])

  rtt_hh_exports <- rtt_hh

  rtt_hh_exports[ix, 1:19] <- rtt_hh_exports[ix, 1:19] + (rtt_hh_exports[ix, 1:19]/z*rtt_hh_exports[ix, "Exports of Goods and Services"])
  rtt_hh_exports[ix, 21:25] <-  rtt_hh_exports[ix, 21:25] + (rtt_hh_exports[ix, 21:25]/z*rtt_hh_exports[ix, "Exports of Goods and Services"])
    # Re-calculate intermediate demand and intermediate inputs
  rtt_hh_exports[ix, "intermediate_demand"] <- rowSums(rtt_hh_exports[ix, 1:19])

  # Re-calculate exports
  rtt_hh_exports[1:19, "Exports of Goods and Services"] <- rtt_hh_exports[1:19, "Total Supply"] - rowSums(rtt_hh_exports[1:19, c(20:25), drop = FALSE])

  # Adjust intermediate inputs
  rtt_hh_exports["Intermediate Inputs", ] <- colSums(rtt_hh_exports[1:19,])

  # Finish adjustments


  # Check Exports -----------------------------------------------------------

  total_employment <- c(total_employment, rep(0, 8))
  local_employment <- c(local_employment, rep(0, 8))

  other_employment <- total_employment - local_employment

  rtt_hh <- rbind(rtt_hh_exports, local_employment, other_employment, total_employment)
  rownames(rtt_hh)[rownames(rtt_hh) %in% c("local_employment", "other_employment", "total_employment")] <- c("Local Employment", "Other Employment", "Total Employment")

  if (type == "basic") {rtt_basic} else if (type == "household") {rtt_hh}


}




