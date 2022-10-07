## code to prepare `create_lq_models` dataset goes here
devtools::load_all()

regions <- get_available_regions()

usethis::use_data(create_lq_models, overwrite = TRUE)
