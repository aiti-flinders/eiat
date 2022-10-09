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

  output_d <- data.frame(
    "Sector" = LETTERS[1:19],
    "Initial/Direct" = sector_impacts,
    "Production Induced" = production_induced_output,
    "Consumption Induced" = consumption_induced_output,
    "Total" = total_output
  )

  #GRP
  rtt_other <- rtt[21:25,1:19]
  rtt_other_per <- rtt_other/rtt[26, 1:19]

  return(output_d)

}
