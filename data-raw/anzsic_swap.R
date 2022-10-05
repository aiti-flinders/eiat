## code to prepare `anzsic_swap` dataset goes here

library(tibble)

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

usethis::use_data(anzsic_swap, internal = TRUE, overwrite = TRUE)
