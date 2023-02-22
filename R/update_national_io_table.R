update_national_io_table <- function() {

  check_updated <- create_19_sector(TRUE)

  if (all(check_updated == eiat::national_19)) {

    cli::cli_alert_success("Package data already up to date")

  } else  {

    cli::cli_alert_warning("Updating Regional Input-Output Tables")

    data <- get_data(year = 2021, data = check_updated)

    lq_models <- create_lq(data, 2021, "household")
    lq_basic <- create_lq(data, 2021, "basic")


    usethis::use_data(lq_models, compress = "gzip", overwrite = TRUE)
    usethis::use_data(lq_basic, compress = "gzip", overwrite = TRUE)

    cli::cli_alert_success("Package data updated")
    return(TRUE)

  }
}

create_lq <- function(data, year, type) {

  regions <- get_available_regions(year = {{year}}) %>%
    dplyr::pull(.data$lga) %>%
    purrr::set_names()

  lq_models <- purrr::map(.x = regions,
                          .f = ~purrr::possibly(rtt_basic, otherwise = "error here")(data, .x, type),
               .progress = TRUE)

  return(lq_models)

}
