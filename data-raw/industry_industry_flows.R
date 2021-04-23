## code to prepare `industry_industry_flows` dataset goes here
library(readabs)
library(dplyr)
library(readxl)
library(tibble)
library(tidyr)
library(stringr)
devtools::load_all()

abs_download <- download_abs_data_cube(catalogue_string = "australian-national-accounts-input-output-tables",
                                  cube = "520905500105.xls",
                                  path = here::here("data-raw"))

#Quadrant 1: Exchanges between explicitly defined industry groups

industry_codes <- read_xls("data-raw/520905500105.xls",
                           sheet = "Table 5",
                           range = "C1:DL1") %>%
  colnames()

q1<- read_xls("data-raw/520905500105.xls",
                 sheet = "Table 5",
                 range = "A4:DL117",
                 col_names = c("from_ioig", "from_industry", industry_codes)) %>%
  select(-from_industry) %>%
  mutate(from_ioig = str_pad(from_ioig, 4, "left", "0"),
         total_industry_uses = rowSums(across(c(2:115)))) %>%
  bind_rows(
    summarise(., across(where(is.character), ~"total_intermediate_uses"),
              across(where(is.double), sum))
  )


#Quadrant 2: Supply balances
q2 <- read_xls("data-raw/520905500105.xls",
               sheet = "Table 5",
               range = "A121:DL126",
               col_names = c("from_ioig", "from_industry", industry_codes)) %>%
  select(-from_industry) %>%
  pivot_longer(cols = 2:length(.), #pivot_longer followed by pivot_wider to transpose q2 to make summing of the columns easier
               names_to = "to_ioig",
               values_to = "flow") %>%
  pivot_wider(names_from = from_ioig,
              values_from = flow) %>%
  mutate(P3 = P3 + P4,
         P4 = P5 + P6) %>%
  select(to_ioig, P1:P4) %>%
  pivot_longer(cols = 2:length(.),
               names_to = "from_ioig",
               values_to = "flow") %>%
  pivot_wider(names_from = to_ioig,
              values_from = flow)


q3 <- read_xls("data-raw/520905500105.xls",
               sheet = "Table 5",
               range = "DN4:DT117",
               col_names = c("household_consumption",
                             "government_consumption",
                             "private_capital",
                             "public_capital",
                             "government_capital",
                             "inventories",
                             "exports")) %>%
  bind_cols(q1 %>% select(from_ioig)) %>%
  relocate(from_ioig) %>%
  mutate(gross_fixed_capital_formation = private_capital + public_capital + government_capital) %>%
  relocate(gross_fixed_capital_formation, .before = "inventories") %>%
  select(!ends_with("_capital"))

q4 <- read_xls("data-raw/520905500105.xls",
               sheet = "Table 5",
               range = "DN121:DT126",
               col_names = c("household_consumption",
                             "government_consumption",
                             "private_capital",
                             "public_capital",
                             "government_capital",
                             "inventories",
                             "exports")) %>%
  mutate(from_ioig = c("P1", "P2", "P3", "P4", "P5", "P6")) %>%
  relocate(from_ioig) %>%
  mutate(gross_fixed_capital_formation = private_capital + public_capital + government_capital) %>%
  relocate(gross_fixed_capital_formation, .before = "inventories") %>%
  select(!ends_with("_capital")) %>%
  pivot_longer(cols = 2:length(.), #pivot_longer followed by pivot_wider to transpose q2 to make summing of the columns easier
               names_to = "to_ioig",
               values_to = "flow") %>%
  pivot_wider(names_from = from_ioig,
              values_from = flow) %>%
  mutate(P3 = P3 + P4,
         P4 = 0) %>%
  select(to_ioig, P1:P4) %>%
  pivot_longer(cols = 2:length(.),
               names_to = "from_ioig",
               values_to = "flow") %>%
  pivot_wider(names_from = to_ioig,
              values_from = flow)


#Create ANZSIC06 19 sector versions of abvoe quadrants

q1_19 <- q1 %>%
  pivot_longer(cols = 2:length(.),
               names_to = "to_ioig",
               values_to = "flow") %>%
  filter(from_ioig != "total_intermediate_uses",
         to_ioig != "total_industry_uses") %>%
  left_join(ioig_anzsic_div, by = c("from_ioig" = "ioig")) %>%
  select(from_anzsic = anzsic_division_code, to_ioig, flow) %>%
  left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
  select(from_anzsic, to_anzsic = anzsic_division_code, flow) %>%
  group_by(from_anzsic, to_anzsic) %>%
  summarise(flow = sum(flow), .groups = "drop") %>%
  pivot_wider(names_from = to_anzsic,
              values_from = flow)

q2_19 <- q2 %>%
  pivot_longer(cols = 2:length(.),
               names_to = "to_ioig",
               values_to = "flow") %>%
  left_join(ioig_anzsic_div, by = c("to_ioig" = "ioig")) %>%
  select(from_anzsic = from_ioig, to_anzsic = anzsic_division_code, flow) %>%
  group_by(from_anzsic, to_anzsic) %>%
  summarise(flow = sum(flow), .groups = "drop") %>%
  pivot_wider(names_from = to_anzsic,
              values_from = flow)


q3_19 <- q3 %>%
  pivot_longer(cols = 2:length(.),
               names_to = "to_ioig",
               values_to = "flow") %>%
  left_join(ioig_anzsic_div, by = c("from_ioig" = "ioig")) %>%
  select(from_anzsic = anzsic_division_code, to_anzsic = to_ioig, flow) %>%
  group_by(from_anzsic, to_anzsic) %>%
  summarise(flow = sum(flow), .groups = "drop") %>%
  pivot_wider(names_from = to_anzsic,
              values_from = flow)


q4_19 <- q4 %>%
  pivot_longer(cols = 2:length(.),
               names_to = "to_anzsic",
               values_to = "flow") %>%
  rename(from_anzsic = from_ioig) %>%
  pivot_wider(names_from = to_anzsic,
              values_from = flow)

q13 <- left_join(q1_19, q3_19) %>%
  mutate(total_industry_uses = rowSums(across(c(2:20)))) %>%
  bind_rows(
    summarise(., across(where(is.character), ~"Total Intermediate Uses"),
              across(where(is.double), sum))
  )

q24 <- left_join(q2_19, q4_19)%>%
  mutate(total_industry_uses = rowSums(across(c(2:20))))

industry_industry_flows_19 <- bind_rows(q13, q24)

aus_production <- industry_industry_flows_19 %>%
  slice(20:24) %>%
  summarise(across(where(is.character), ~"Australian Production"),
            across(where(is.double), sum))

industry_industry_flows_19 <- bind_rows(industry_industry_flows_19,
                                        aus_production,
                                        employment_industry_fy("2017-18")) %>%
  replace_na(list(exports = 0,
                  government_consumption = 0,
                  gross_fixed_capital_formation = 0,
                  household_consumption = 0,
                  inventories = 0,
                  total_industry_uses = 0)) %>%
  mutate(total_supply = rowSums(across(c(21:26))))





usethis::use_data(industry_industry_flows_19, overwrite = TRUE)
