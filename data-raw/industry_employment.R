## code to prepare `industry_employment` dataset goes here
library(aitidata)
library(dplyr)
library(tidyr)

aitidata::employment_by_industry %>%
  filter(between(date, as.Date("2017-07-01"), as.Date("2018-06-30")),
         gender == "Persons",
         age == "Total (age)",
         state == "Australia",
         indicator %in% c("Employed full-time", "Employed total"),
         industry != "Total (Industry)") %>%
  group_by(indicator, industry) %>%
  summarise(value = sum(value)) %>%
  pivot_wider(names_from = industry) %>%
  rename(from_anzsic = indicator)

usethis::use_data(industry_employment, overwrite = TRUE)
