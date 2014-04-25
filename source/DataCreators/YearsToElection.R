##############
# Years to election data download
# Christopher Gandrud
# 24 April 2014
##############

library(psData)

# Years left in current term
YearsLeft <- DpiGet(vars = c('yrcurnt', 'stabns'))
YearsLeft$iso2c[YearsLeft$iso2c == 'GB'] <- 'UK'
YearsLeft <- YearsLeft[, -2]
names(YearsLeft) <- c('country', 'year', 'yrcurnt', 'stanbs')

# Load costs data (used to keep only included observations)
EUCosts <- read.csv('/git_repositories/CrisisDataIssues/data/Eurostat_CrisisCosts.csv',
                    stringsAsFactors = FALSE)

Comb <- merge(EUCosts, YearsLeft, Var = c('country', 'year'), all.x = TRUE)

# Keep DPI data 
Comb <- Comb[, c('country', 'year', 'yrcurnt', 'stanbs')]

## Recode focusing on parliamentary elections, if executive is a figurehead
## Recode all election years as 0
## From the European Election Database (http://www.nsd.uib.no/european_election_database)
## Only changed for countries that responded to financial a crisis

## Also updated for 2013 using the European Elections Database

# Austrian parliamentary elections rather than executive
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2007] <- 1
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2008] <- 0
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2009] <- 4
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2010] <- 5
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2011] <- 2
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2012] <- 1
Comb$yrcurnt[Comb$country == 'AT' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'BE' & Comb$year == 2010] <- 0
Comb$yrcurnt[Comb$country == 'BE' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'BG' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'CY' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'CZ' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'DE' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'DK' & Comb$year == 2007] <- 0
Comb$yrcurnt[Comb$country == 'DK' & Comb$year == 2013] <- 2

Comb$yrcurnt[Comb$country == 'EE' & Comb$year == 2013] <- 3

Comb$yrcurnt[Comb$country == 'ES' & Comb$year == 2011] <- 0
Comb$yrcurnt[Comb$country == 'ES' & Comb$year == 2013] <- 2

Comb$yrcurnt[Comb$country == 'FI' & Comb$year == 2013] <- 2

Comb$yrcurnt[Comb$country == 'FR' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'GR' & Comb$year == 2007] <- 0
Comb$yrcurnt[Comb$country == 'GR' & Comb$year == 2009] <- 0
Comb$yrcurnt[Comb$country == 'GR' & Comb$year == 2012] <- 0
Comb$yrcurnt[Comb$country == 'GR' & Comb$year == 2013] <- 3

Comb$yrcurnt[Comb$country == 'HR' & Comb$year == 2013] <- 2

Comb$yrcurnt[Comb$country == 'HU' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'IE' & Comb$year == 2011] <- 0
Comb$yrcurnt[Comb$country == 'IE' & Comb$year == 2013] <- 3

Comb$yrcurnt[Comb$country == 'IT' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2007] <- 1
Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2008] <- 0
Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2009] <- 3
Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2010] <- 2
Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2011] <- 1
Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2012] <- 0
Comb$yrcurnt[Comb$country == 'LT' & Comb$year == 2013] <- 3

Comb$yrcurnt[Comb$country == 'LU' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2007] <- 3
Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2008] <- 2
Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2009] <- 1
Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2010] <- 0
Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2011] <- 0
Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2012] <- 2 # limited elections term when early elections called
Comb$yrcurnt[Comb$country == 'LV' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'MT' & Comb$year == 2013] <- 0

Comb$yrcurnt[Comb$country == 'NL' & Comb$year == 2012] <- 0
Comb$yrcurnt[Comb$country == 'NL' & Comb$year == 2013] <- 3

Comb$yrcurnt[Comb$country == 'PL' & Comb$year == 2013] <- 2

Comb$yrcurnt[Comb$country == 'PT' & Comb$year == 2011] <- 0
Comb$yrcurnt[Comb$country == 'PT' & Comb$year == 2013] <- 2

Comb$yrcurnt[Comb$country == 'RO' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'SE' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'SI' & Comb$year == 2008] <- 0
Comb$yrcurnt[Comb$country == 'SI' & Comb$year == 2011] <- 0
Comb$yrcurnt[Comb$country == 'SI' & Comb$year == 2013] <- 3

Comb$yrcurnt[Comb$country == 'SK' & Comb$year == 2013] <- 1

Comb$yrcurnt[Comb$country == 'UK' & Comb$year == 2013] <- 2

#YearsLeft$DiElection[YearsLeft$yrcurnt == 0] <- "Election Year"
#YearsLeft$DiElection[YearsLeft$yrcurnt > 0] <- "Not Election Year"

# Save data
SaveDir <- '/git_repositories/CrisisDataIssues/data/'
write.csv(Comb, file = paste0(SaveDir, 'Elections.csv'), row.names = FALSE)
