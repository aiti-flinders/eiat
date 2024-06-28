# eiat 0.3.0

## Data updates

* Regional input-output tables are now derived using the location quotients from their relevant state. The state tables are calculated based on the national input-output table published by the ABS (https://www.abs.gov.au/statistics/economy/national-accounts/australian-national-accounts-input-output-tables/2021-22)

## Bug fixes

* The national input-output table has been updated to 115 sectors. IOIG/IOPG 6701 Ownership of Dwellings has been split into 6700 Imputed rent for owner-occupiers and 6701 Actual rent for housing. This change prevented updates to the 19 (ANZSIC) sector input-output table. 



# eiat 0.2.2

* New `eiat()` incorporates the eiat shiny application into this package. 

* Employment by industry figures are once again in numbers of people, not '000s of people. 
