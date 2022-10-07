## code to prepare `create_lq_models` dataset goes here
devtools::load_all()

nat_data <- get_data(year = 2016)

lq_models <- get_available_regions(year = 2016) %>%
  set_names() %>%
  map(~possibly(rtt_basic, otherwise = "error here")(nat_data, .x))



usethis::use_data(lq_models, overwrite = TRUE)
