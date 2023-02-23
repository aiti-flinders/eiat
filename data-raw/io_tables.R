## code to prepare `national_19` dataset goes here
library(eiat)
national_19 <- create_19_sector()

usethis::use_data(national_19, compress = "gzip", overwrite = TRUE)

