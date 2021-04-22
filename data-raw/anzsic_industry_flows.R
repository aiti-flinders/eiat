## code to prepare `anzsic_industry_flows` dataset goes here
library(abscorr)
library(dplyr)
library(tidyr)
library(stringr)
library(fuzzyjoin)

anzsic_industry_flows <- industry_industry_flows %>%
  pivot_longer(cols = 2:length(.),
               names_to = "to_ioig",
               values_to = "flow") %>%
  left_join(ioig_anzsic_div, by = c("from_ioig" = "ioig")) %>%
  rename(from_anzsic = anzsic_division) %>%
  group_by(from_ioig) %>%
  mutate(from_anzsic = case_when(
    is.na(from_anzsic) ~ ioig_anzsic_div$anzsic_division,
    TRUE ~ from_anzsic)
  )

%>%
  select(from_anzsic, to_ioig, flow) %>%
  left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
  rename(to_anzsic = anzsic_division) %>%
  select(from_anzsic, to_anzsic, flow)

distinct(anzsic_industry_flows, from_industry, ioig_descriptor)

#usethis::use_data(anzsic_industry_flows, overwrite = TRUE)
