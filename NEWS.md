# eiat 0.3.0

## Data Updates

* Regional input-output tables are now derived using the location quotients from their relevant state. The state tables are calculated based on the national input-output table published by the ABS (https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/2021-22)

## Bug Fixes

* The national input-output table has been updated to 115 sectors. IOIG/IOPG 6701 Ownership of Dwellings has been split into 6700 Imputed rent for owner-occupiers and 6701 Actual rent for housing. This change prevented updates to the 19 (ANZSIC) sector input-output table. 

* The first column of the matrix to input expenditure data defaults to the current year. 


# eiat 0.2.2

* New `eiat()` incorporates the eiat shiny application into this package. 

## Bug Fixes

* Employment by industry figures are once again in numbers of people, not '000s of people. 
