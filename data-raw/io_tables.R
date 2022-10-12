## code to prepare `io_tables` dataset goes here
library(eiat)

industry_flows <- read_industry_flow_table()
national_employment <- read_national_employment_table()
national_19 <- create_19_sector()

usethis::use_data(national_19, compress = "xz", overwrite = TRUE)
usethis::use_data(industry_flows, compress = "xz", overwrite = TRUE)
usethis::use_data(national_employment, compress = "xz", overwrite = TRUE)
