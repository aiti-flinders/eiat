## code to prepare `ioig_anzsic` dataset goes here
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(strayr)
library(readxl)

download.file(url = "https://www.abs.gov.au/methodologies/australian-national-accounts-input-output-tables-methodology/2019-20/Industry%20and%20Product%20Concordance%20Tables%202019-20.xlsx",
                          destfile = "data-raw/ioig_anzsic.xlsx",
                          mode = "wb")

ioig_anzsic <- read_xlsx("data-raw/ioig_anzsic.xlsx",
                        sheet = "IOIG(2015) to ANZSIC06",
                        range = "A5:D625",
                        col_types = c("text", "text", "numeric", "text")) %>%
  clean_names() %>%
  filter(!is.na(anzsic_code)) %>%
  fill(ioig, ioig_descriptor, .direction = "down") %>%
  mutate(across(c(ioig, anzsic_code), ~str_pad(.x, 4, "left", "0"))) %>%
  add_row(ioig = "1205", ioig_descriptor = "Wine, spirits and tobacco", anzsic_code = "1213", anzsic_descriptor = "Spirit Manufacturing") %>%
  add_row(ioig = "1205", ioig_descriptor = "Wine, spirits and tobacco", anzsic_code = "1214", anzsic_descriptor = "Wine and Other Alcoholic Beverage Manufacturing") %>%
  add_row(ioig = "1205", ioig_descriptor = "Wine, spirits and tobacco", anzsic_code = "1220", anzsic_descriptor = "Cigarette and Tobacco Product Manufacturing")

ioig_anzsic_div <- left_join(ioig_anzsic, anzsic2006 %>% mutate(anzsic_class_code = str_pad(anzsic_class_code, 4, "left", "0")), by = c("anzsic_code" = "anzsic_class_code")) %>%
  distinct(ioig, anzsic_division_code)

usethis::use_data(ioig_anzsic_div, overwrite = TRUE)
