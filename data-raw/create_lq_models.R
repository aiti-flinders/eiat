## code to prepare `create_lq_models` dataset goes here
devtools::load_all()

nat_data <- get_data(year = 2021)

create_lq <- function(data, year, type) {


  lq_models <- get_available_regions(year = {{year}}) %>%
    dplyr::pull(lga) %>%
    purrr::set_names() %>%
    purrr::map(~purrr::possibly(rtt_basic, otherwise = "error here")(data, .x, type))

  return(lq_models)
}


lq_models <- create_lq(nat_data, 2021, "household")
lq_basic <- create_lq(nat_data, 2021, "basic")


usethis::use_data(lq_models, overwrite = TRUE)
usethis::use_data(lq_basic, overwrite = TRUE)
