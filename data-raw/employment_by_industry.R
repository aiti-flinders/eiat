library(dplyr)
library(readr)
library(glue)
library(purrr)
library(forcats)

#Regional Employment (LGA)

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

lga_emp <- map(.x = c(2011, 2016, 2021),
                    .f = ~regional_employment(year = .x)) |>
  list_rbind() |>
  mutate(across(c(lga_ur, lga_pow), ~gsub("\\^", "\\'", .x))) |>
  filter(if_all(c(lga_ur, lga_pow), ~ !grepl("No Fixed Address|POW not applicable", .x)))


# Regional employment (State)

renamer_state <- function(colnames) {

  dplyr::case_when(
    grepl("INDP", colnames) ~ "industry",
    grepl("\\(POW\\)", colnames) ~ "state_pow",
    grepl("STATE|\\(UR\\)", colnames) ~ "state_ur",
    grepl("Counting", colnames) ~ "drop",
    grepl("Count", colnames) ~ "employment",
    TRUE ~ "extra"
  )

}

state_employment <- function(year) {
  read_csv(glue("data-raw/Employment by State by POW by Industry by UR {year} .csv"),
                             skip = 9)  |>
  rename_with(renamer_state) |>
  select(-drop, -extra) |>
  mutate(year = 2021) |>
  filter(!is.na(state_ur),
         if_all(c(industry, state_ur, state_pow), ~. != "Total"))
}

state_emp <- map(.x = c(2021),
          .f = ~state_employment(year = .x)) |>
  list_rbind() |>
  mutate(across(c(state_ur, state_pow), ~gsub("\\^", "\\'", .x))) |>
  filter(if_all(c(state_ur, state_pow), ~ !grepl("No Fixed Address|POW not applicable", .x)))




# Combine data

live_and_work <- list(
  lga = lga_emp |> filter(lga_ur == lga_pow),
  state = state_emp |> filter(state_ur == state_pow)
)

work <- list(
  lga = lga_emp |>
    group_by(lga_pow, industry, year) |>
    summarise(employment = sum(employment),
              .groups = "drop")  |>
    mutate(industry = fct_relevel(industry, unique(live_and_work$lga$industry))) |>
    arrange(industry),
  state = state_emp |>
    group_by(state_pow, industry, year) |>
    summarise(employment = sum(employment),
              .groups = "drop") |>
    mutate(industry = fct_relevel(industry, unique(live_and_work$lga$industry))) |>
    arrange(industry)
)

# Lets take a look at IOIG employment
# IOIG is concorded to ANZSIC at the 4 digit level.

state_emp_subdivision <- read_csv("data-raw/Employment by State by POW by Industry 2 by UR 2021.csv",
                           skip = 9,
                           col_select = c("state_pow" = "STATE (POW)",
                                          "state_ur" = "STATE (UR)",
                                          "anzsic_code" = "2-digit level INDP Industry of Employment",
                                          "employment" = Count)) |>
  mutate(year = 2021) |>
  filter(!is.na(state_ur),
         if_all(c(anzsic_code, state_ur, state_pow), ~. != "Total"))


usethis::use_data(live_and_work, compress = "xz", overwrite = TRUE)
usethis::use_data(work, compress = "xz", overwrite = TRUE)
