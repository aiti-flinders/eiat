## code to prepare `regions` dataset goes here
library(readxl)
library(dplyr)

read_regions <- function(year) {

  path <- paste0("data-raw/LGA_", year, "_AUST.xlsx")

  read_excel(path) %>%
    distinct(LGA_NAME_2022, STATE_NAME_2021) %>%
    filter(!grepl("Migratory|No usual address|Unincorp|Other Australia", LGA_NAME_2022)) %>%
    rename(lga = LGA_NAME_2022,
           state = STATE_NAME_2021) %>%
    mutate(year = {{year}})

}

regions <- read_regions(2022)


usethis::use_data(regions,  overwrite = TRUE)
