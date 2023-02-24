## code to prepare `internal_data` dataset goes here
library(eiat)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readxl)
library(sf)
library(strayr)


# Industry Flows ----------------------------------------------------------

industry_flows <- eiat:::read_industry_flow_table()

# National Employment -----------------------------------------------------

national_employment <- eiat:::read_national_employment_table()

# Input Output Industry Group to ANZSIC -----------------------------------

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




# ANZSIC Letters to Numbers -----------------------------------------------

l <- LETTERS[1:19]
n <- c("Agriculture, Forestry and Fishing",
       "Mining",
       "Manufacturing",
       "Electricity, Gas, Water and Waste Services",
       "Construction",
       "Wholesale Trade",
       "Retail Trade",
       "Accommodation and Food Services",
       "Transport, Postal and Warehousing",
       "Information Media and Telecommunications",
       "Financial and Insurance Services",
       "Rental, Hiring and Real Estate Services",
       "Professional, Scientific and Technical Services",
       "Administrative and Support Services",
       "Public Administration and Safety",
       "Education and Training",
       "Health Care and Social Assistance",
       "Arts and Recreation Services",
       "Other Services"
)
anzsic_swap <- tibble(letter = l, name = n)


# Input Output Column / Row Names -----------------------------------------

io_cols <- read_excel("data-raw/520905500105.xlsx",
                      sheet = "Table 5",
                      range = "C2:DV2")
io_cols_names <- c(colnames(read_excel("data-raw/520905500105.xlsx",
                                       sheet = "Table 5",
                                       range = "C1:DL1")),
                   "T4", "Q1", "Q2", "Q3", "Q4", "Q5", "Q6", "Q7", "T5", "T6")

io_rows <- read_excel("data-raw/520905500105.xlsx",
                      sheet = "Table 5",
                      range = "B4:B130",
                      col_names = c("columns")) %>%
  filter(!is.na(columns)) %>%
  pull(columns)

io_rows_names <- c(read_excel("data-raw/520905500105.xlsx",
                              sheet = "Table 5",
                              range = "A4:A130",
                              col_names = c("columns")) %>%
                     filter(!is.na(columns)) %>%
                     mutate(columns = str_pad(columns, 4, "left", "0")) %>%
                     pull(columns),
                   "Australian Production",
                   "Value Added")


io_cols <- colnames(io_cols)
io_cols <- gsub("\r\n", "", io_cols)
io_cols <- gsub("; ", "", io_cols)

names(io_cols) <- io_cols_names
names(io_rows) <- io_rows_names

usethis::use_data(industry_flows,
                  national_employment,
                  ioig_anzsic_div,
                  lga_boundaries,
                  anzsic_swap,
                  io_cols,
                  io_rows,
                  compress = "xz",
                  internal = TRUE,
                  overwrite = TRUE)
