## code to prepare `employment` datasets goes here
library(readxl)
library(tidyr)

local_employment <- read_excel("data-raw/EIAT-POW_industry_employment_2016-LiveWorkInRegion.xlsx",
                           sheet = "Raw",
                           range = "B11:Z5412",
                           col_names = c("place_of_work", "usual_residence", LETTERS[1:19], "Inadequately described", "Not stated", "Not applicable", "Total")) %>%
  tidyr::fill(place_of_work, .direction = "down")

read_lga_names <- read_excel("data-raw/EIAT-POW_industry_employment_2016-WorkInRegion.xlsx",
                             sheet = "Raw",
                             range = "C9:BX9") %>%
  colnames()

regional_employment <- read_excel("data-raw/EIAT-POW_industry_employment_2016-WorkInRegion.xlsx",
                                  sheet = "Raw",
                                  range = "B11:BX33",
                                  col_names = c("anzsic_division_code", read_lga_names)) %>%
  pivot_longer(cols = 2:length(.), names_to = "lga", values_to = "jobs")  %>%
  pivot_wider(names_from = anzsic_division_code, values_from = jobs)

colnames(regional_employment) <- c("lga", LETTERS[1:19], "Inadequately described", "Not stated", "Not applicable", "Total")

usethis::use_data(local_employment, overwrite = TRUE)
usethis::use_data(regional_employment, overwrite = TRUE)
