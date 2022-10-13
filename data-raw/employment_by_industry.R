library(dplyr)
library(readr)
library(glue)
library(purrr)
library(forcats)

renamer <- function(colnames) {

  dplyr::case_when(
    grepl("INDP", colnames) ~ "industry",
    grepl("\\(POW\\)", colnames) ~ "lga_pow",
    grepl("LGA|\\(UR\\)", colnames) ~ "lga_ur",
    grepl("Counting", colnames) ~ "drop",
    grepl("Count", colnames) ~ "employment",
    TRUE ~ "extra"
  )

}

regional_employment <- function(year) {
  read_csv(glue("data-raw/Employment by LGA by POW by Industry by UR {year}.csv"),
           skip = 9) %>%
    rename_with(renamer) %>%
    select(-drop, -extra) %>%
    mutate(year = {{year}}) %>%
    filter(!is.na(lga_ur),
           if_all(c(industry, lga_ur, lga_pow), ~. != "Total"))
}

df <- purrr::map_df(.x = c(2011, 2016, 2021),
                    .f = ~regional_employment(year = .x))

df <- df %>%
  mutate(across(c(lga_ur, lga_pow), ~gsub("\\^", "\\'", .x))) %>%
  filter(if_all(c(lga_ur, lga_pow), ~ !grepl("No Fixed Address|POW not applicable", .x)))

live_and_work <- df %>%
  filter(lga_ur == lga_pow)

work <- df %>%
  group_by(lga_pow, industry, year) %>%
  summarise(employment = sum(employment),
            .groups = "drop") %>%
  mutate(industry = fct_relevel(industry, unique(live_and_work$industry))) %>%
  arrange(industry)

usethis::use_data(live_and_work, compress = "xz", overwrite = TRUE)
usethis::use_data(work, compress = "xz", overwrite = TRUE)
