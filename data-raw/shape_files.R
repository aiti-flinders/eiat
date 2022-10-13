library(sf)
library(strayr)

lga_2011 <- read_absmap("lga2011") %>%
  st_drop_geometry() %>%
  select(lga = lga_name_2011,
         state_name = state_name_2011)

lga_2016 <- read_absmap("lga2016") %>%
  st_drop_geometry() %>%
  select(lga = lga_name_2016,
         state_name = state_name_2016)

lga_2021 <- read_absmap("lga2021") %>%
  st_drop_geometry() %>%
  select(lga = lga_name_2021,
         state_name = state_name_2021)


usethis::use_data(lga_2011, compress = "xz", overwrite = TRUE)
usethis::use_data(lga_2016, compress = "xz", overwrite = TRUE)
usethis::use_data(lga_2021, compress = "xz", overwrite = TRUE)

