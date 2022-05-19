#' Create basic transaction table for a region
#'
#' @param region
#'
#' @return
#' @export
#'
#' @examples
rtt_basic <- function(region) {

  iif_m <- industry_industry_flows_19 %>%
    dplyr::filter(!from_anzsic %in% c("Total Intermediate Uses", "Employed total")) %>%
    dplyr::select(-total_industry_uses) %>%
    dplyr::relocate(c(household_consumption, government_consumption, gross_fixed_capital_formation, inventories, exports, total_supply), .after = S) %>%
    tibble::column_to_rownames("from_anzsic") %>%
    as.matrix()

  #Calculate Australian DC
  direct_coefficients <-  iif_m %*% diag(1/iif_m["Australian Production",], names = TRUE)
  colnames(direct_coefficients) <- colnames(iif_m)

  #Calculate Regions DC
  regional_direct_coefficients <- diag(lq(region)) %*% direct_coefficients[1:19,]
  rownames(regional_direct_coefficients) <- rownames(iif_m[1:19,])

  #Add P1-P4 to Regional DC - they're the same as in the national DC
  regional_direct_coefficients <- rbind(regional_direct_coefficients, direct_coefficients[20:24,])
  #Imports (P4) is replaced as the balance of production (which has a DC of 1) and the sum of the transactions
  regional_direct_coefficients["P4", ] <- 1 - colSums(regional_direct_coefficients[1:22, ])

  #HH "Productivity" and ""Other"" """Productivity"""  - this section is weird and needs better documentation
  hh_productivity <- iif_m["Australian Production", "household_consumption"] / iif_m["P1", "total_supply"]
  other_productivity <- iif_m["Australian Production", c("government_consumption", "gross_fixed_capital_formation", "inventories")]/iif_m["Australian Production", "total_supply"]

  compensation_all <- regional_direct_coefficients[,1:19] %*% diag(sector_productivity(region))

  hh_productivity <- hh_productivity*sum(compensation_all["P1", ])
  other_productivity <- other_productivity*(get_regional_employment(region) %>%
                                              fte_employment() %>%
                                              dplyr::summarise(sum(adjust_fte)) %>%
                                              dplyr::pull())

  all_productivity <- c(sector_productivity(region), hh_productivity, other_productivity)

  rtt_basic <- regional_direct_coefficients[,1:23] %*% diag(all_productivity)
  rtt_basic <- cbind(rtt_basic, c(sector_productivity(region), rep(NA, 5)))
  rtt_basic[c("P1", "P2", "P3", "P4"), 24] <- rowSums(rtt_basic[c("P1", "P2", "P3", "P4"), ], na.rm = T)
  rtt_basic["Australian Production", 24] <- sum(rtt_basic[,24], na.rm = T)

  rtt_exports <- rtt_basic[, 24] - rowSums(rtt_basic[, 1:23])

  rtt_basic <- cbind(rtt_basic, rtt_exports)
  #Swap columns
  rtt_basic[, c(24,25)] <- rtt_basic[, c(25,24)]
  colnames(rtt_basic) <- colnames(direct_coefficients)



  return(rtt_basic)


}




