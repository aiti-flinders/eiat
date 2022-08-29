## code to prepare `114_sector_names` dataset goes here
library(readxl)
library(stringr)
library(dplyr)

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

usethis::use_data(io_cols, overwrite = TRUE)
usethis::use_data(io_rows, overwrite = TRUE)
