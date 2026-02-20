# regenerate internal growthstandard datasets from data-raw
setwd('.')
source('data-raw/growthstandards/growthstandards.R')
cat('Wrote internal growthstandard data via usethis::use_data()\n')
