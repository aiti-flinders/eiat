library(sf)
library(absmapsdata)

lga_2011 <- lga2011 %>%
  st_drop_geometry() %>%
  select(lga = lga_name_2011,
         state_name = state_name_2011)

lga_2016 <- lga2016 %>%
  st_drop_geometry() %>%
  select(lga = lga_name_2016,
         state_name = state_name_2016)

usethis::use_data(lga_2011, compress = "xz", overwrite = TRUE)
usethis::use_data(lga_2016, compress = "xz", overwrite = TRUE)
