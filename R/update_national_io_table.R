#' Update National Input Output Table
#'
#' @param progress logical. TRUE to show a progress bar
#'
#' @return TRUE
#' @export
#'
update_national_io_table <- function(progress = TRUE) {

  check_updated <- create_19_sector(TRUE)

  if (all(check_updated == eiat::national_19)) {

    cli::cli_alert_success("Package data already up to date")
    return(TRUE)

  } else  {

    cli::cli_alert_warning("Updating Regional Input-Output Tables")

    data <- get_data(year = 2021, data = check_updated)

    lq_models <- create_lq(data, "household", progress)
    lq_basic <- create_lq(data, "basic", progress)

    national_19 <- create_19_sector(TRUE)

    usethis::use_data(national_19, compress = "xz", overwrite = TRUE)
    usethis::use_data(lq_models, compress = "gzip", overwrite = TRUE)
    usethis::use_data(lq_basic, compress = "gzip", overwrite = TRUE)

    cli::cli_alert_success("Package data updated")
    return(TRUE)

  }
}

#' Create Location Quotient model input output tables
#'
#' @param data data.frame from get_data()
#' @param type character.
#' @param progress logical. TRUE to show a progress bar
#'
#' @return a list of matrices
#' @export
#'
create_lq <- function(data,  type, progress) {

  regions <- get_available_regions() %>%
    dplyr::pull(.data$lga) %>%
    purrr::set_names()


  lq_models <- purrr::map(.x = regions,
                          .f = ~purrr::safely(rtt_basic)(data, .x, type),
                          .progress = progress)

  lq_models <- purrr::transpose(lq_models)

  if (any(!is.null(unlist(lq_models[["error"]])))) {

    return(lq_models)

  } else {

    lq_models <- lq_models[["result"]]
    return(lq_models)
  }

}
