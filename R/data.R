#' Employment (by Place of Work) by Industry data
#'
#' Total employment by industry by place (local government area) of work for ABS Census years 2011
#' 2016 and 2021. ABS TableBuilder Pro
#'
#' @format ## `work`
#' A data frame with 37,312 rows and 4 columns.
#' \describe{
#' \item{lga_pow}{Place (Local Government Area) of work}
#' \item{industry}{ANZSIC 19 Sector Industry}
#' \item{year}{Census year}
#' \item{employment}{Total employment}
#' }
#'
"work"

#' Employment (by Place of Work, by Place of Residence) by Industry data
#'
#' Total employment by industry, by place of work and by place of usual residence (based on Local Government Area boundies)
#' for ABS Census years 2011, 2016 and 2021. From ABS TableBuilder Pro. Only includes employment where people live and work in the
#' same LGA.
#'
#' @format ## `live_and_work`
#' A data frame with 36,564 rows and 5 columns.
#' \describe{
#' \item{industry}{ANZSIC 19 Sector Industry}
#' \item{lga_ur}{Place (Local Government Area) of usual residency}
#' \item{lga_pow}{Place (Local Government Area) of work}
#' \item{employment}{Total employment}
#' \item{year}{Census year}
#' }
"live_and_work"

#' Australian 19 Sector Input-Output Table
#'
"national_19"

#' LGA 2022 State Names
#'
"regions"

#' Regional Input Output Tables (Household Split)
#'
#' Regional Input-Output tables derived using the location quotient method from the Australian 19 Sector Input-Output table.
#' These tables split household expenditure and wages and salaries by those who live and work in the region and outside the region.
#'
#' @format ## `lq_models`
#' A list of 556 matrices with 29 rows and 27 columns.
"lq_models"

#' Regional Input Output Tables
#'
#' Regional Input-Output tables derived using the location quotient method from the Australian 19 Sector Input-Output table.
#'
#' @format ## `lq_basic`
#' A list of 556 matrices with 29 rows and 27 columns.
"lq_basic"


