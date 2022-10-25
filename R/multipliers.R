multipliers <- function(region) {

  rtt <- lq_models[[region]]

  basic_price <- rtt[c(1:19,21,22), c(1:19, 21:22)]
  total <- rtt[c("Australian Production"), c(1:19, 21:22)]

  A <- basic_price[1:19, 1:19] %*% diag(ifelse(is.infinite(1/total[1:19]), 0, 1/total[1:19]), names = TRUE)

  I <- diag(1, 19, 19)

  inv_I_A <- solve(I - A)
  rownames(inv_I_A) <- colnames(inv_I_A) <- anzsic_swap$name
  inv_I_A <- rbind(inv_I_A, colSums(inv_I_A))


  A_closed <- basic_price[1:20, 1:20] %*% diag(ifelse(is.infinite(1/total[1:20]), 0, 1/total[1:20]), names = TRUE)

  I <- diag(1, 20, 20)

  inv_I_A_bar <- solve(I - A_closed)

  rownames(inv_I_A_bar) <- rownames(basic_price[1:20, 1:20])
  colnames(inv_I_A_bar) <- colnames(basic_price[1:20, 1:20])


  inv_I_A_bar <- rbind(inv_I_A_bar, colSums(inv_I_A_bar))


# Output ------------------------------------------------------------------

  initial <- rep(1, 19)
  production_induced <- inv_I_A[20, ] - 1
  consumption_induced <- inv_I_A_bar[21, 1:19] - inv_I_A[20, ]
  type_1 <- (initial + production_induced)/initial
  type_2 <- (initial + production_induced + consumption_induced) / initial

  output_multipliers <- data.frame(
    fix.empty.names = FALSE,
    row.names = NULL,
    "Sector" = names(production_induced),
    "Direct" = initial,
    "Production Induced" = production_induced,
    "Consumption Induced" = consumption_induced,
    "Type I Multiplier" = type_1,
    "Type II Multiplier" = type_2
  )


# GRP ---------------------------------------------------------------------
  fte_hh_v <- rtt[c("Wages and Salaries - Local",
                    "Wages and Salaries - Other",
                    "Gross operating surplus and mixed income",
                    "Taxes less subsidies on products and production",
                    "Local Employment",
                    "Other Employment",
                    "Total Employment"),1:19] %*% diag(ifelse(is.infinite(1/rtt["Australian Production", 1:19]), 0, 1/rtt["Australian Production", 1:19]), names = T)

  grp <- colSums(fte_hh_v[1:4, ])

  fte_hh_v <- rbind(fte_hh_v, grp)

  production_induced <- colSums(inv_I_A[1:19, 1:19]*grp) - grp
  consumption_induced <- colSums(inv_I_A_bar[1:19, 1:19]*grp) - colSums(inv_I_A[1:19, 1:19]*grp)


  grp_multipliers <- data.frame(
    fix.empty.names = FALSE,
    row.names = NULL,
    "Sector" = anzsic_swap$name,
    "Direct" = grp,
    "Production Induced" = production_induced,
    "Consumption Induced" = consumption_induced,
    "Type I" = (grp + production_induced)/grp,
    "Type II" = (grp + production_induced + consumption_induced) / grp
  )



# Employment --------------------------------------------------------------


  production_induced <- colSums(inv_I_A[1:19, 1:19]*fte_hh_v["Total Employment", ]) - fte_hh_v["Local Employment", ]
  consumption_induced <- colSums(inv_I_A_bar[1:19, 1:19]*fte_hh_v["Total Employment",]) - colSums(inv_I_A[1:19, 1:19]*fte_hh_v["Total Employment", ])

  emp_multipliers <- data.frame(
    fix.empty.names = FALSE,
    row.names = NULL,
    "Sector" = anzsic_swap$name,
    "Direct" = fte_hh_v["Total Employment", ],
    "Production Induced" = production_induced,
    "Consumption Induced" = consumption_induced,
    "Type I" = (fte_hh_v["Total Employment", ] + production_induced) / fte_hh_v["Total Employment", ],
    "Type II" = (fte_hh_v["Total Employment", ] + production_induced + consumption_induced) / fte_hh_v["Total Employment", ]
  )

  out <- list("output" = output_multipliers,
              "grp" = grp_multipliers,
              "emp" = emp_multipliers)

  return(out)
}
