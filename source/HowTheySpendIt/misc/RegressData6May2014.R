################
# Create basic EU data set for simple test regressions
# Christopher Gandrud
# 6 May 2014
###############

# Load packages
library(DataCombine)
library(WDI)
library(foreign)
library(dplyr)

# Set directories
# Set data directory
DD <- '/git_repositories/CrisisDataIssues/data/'

# Save directory
SD <- '/git_repositories/CrisisDataIssues/data/misc/'

#### Eurostat ####

# Eurostat data was gathered from: http://epp.eurostat.ec.europa.eu/portal/page/portal/ government_finance_statistics/excessive_deficit/supplementary_tables_financial_turmoil. Accessed April 2014.
# The data was transformed using into the current format using: 
# https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/EU_CostsData.R

# Load data and clean
EUCosts <- read.csv(paste0(DD, 'Eurostat_CrisisCosts.csv'),
                    stringsAsFactors = FALSE)


# Load modified election timing variable
## Created using:
## https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/YearsToElection.R

YearsLeft <- read.csv(paste0(DD, 'Elections.csv'), stringsAsFactors = FALSE)

Comb <- dMerge(EUCosts, YearsLeft, Var = c('country', 'year'), all.x = TRUE)

Comb$CollapseElect <- Comb$yrcurnt
Comb$CollapseElect[Comb$yrcurnt > 1] <- 3
Comb$CollapseElect <- factor(Comb$CollapseElect, 
                             labels = c('Election Year', 'Elect. - 1', 
                                        'Other Years'))


# Remove Finland (only has one observation year)
Comb <- subset(Comb, country != 'FI')

# Remove Slovakia (only has two years)
Comb <- subset(Comb, country != 'SK')

Comb <- DropNA(Comb, 'ContingentLiabilities')

# Find country standard deviations in liabilities
SDMean <- function(var){
    
    Comb <- eval(parse(text = paste0('mutate(group_by(Comb, country), tempMean = mean(',
                                     var,'))')))
    Comb <- eval(parse(text = paste0('mutate(group_by(Comb, country), tempSD = sd(',
                                     var,'))')))
    
    Comb$tempMSub <- Comb[, var] - Comb[, 'tempMean']
    Comb[, paste0(var, '_Standard')] <- Comb$tempMSub/Comb$tempSD
    Comb <- VarDrop(Comb, c('tempMean', 'tempSD', 'tempMSub'))
    
    return(Comb)
}

Comb <- SDMean('ContingentLiabilities')
Comb <- SDMean('GovLiabilities')

#### Add NPLs ####
wdiData <- WDI(indicator = c('FB.AST.NPER.ZS', 'NY.GDP.MKTP.KD.ZG'), 
           start = 2003, end = 2013)

wdiData <- wdiData[, c('iso2c', 'year', 'FB.AST.NPER.ZS', 
                        'NY.GDP.MKTP.KD.ZG')]

names(wdiData) <- c('country', 'year', 'wdiNpl', 'wdiGrowth')

wdiData$country[wdiData$country == 'GB'] <- 'UK'

CountriesIncluded <- Comb$country

wdiData <- wdiData[wdiData$country %in% CountriesIncluded, ]

# Merbe with main data
Comb <- dMerge(Comb, wdiData, Var = c('country', 'year'), all = TRUE)
Comb <- Comb[order(Comb$country, Comb$year), ]

# Create lagged variables
Comb <- slide(Comb, 'wdiNpl', GroupVar = 'country', slideBy = -1,
              NewVar = 'NPL_Lag1')
Comb <- slide(Comb, 'wdiNpl', GroupVar = 'country', slideBy = -2,
              NewVar = 'NPL_Lag2')

Comb <- slide(Comb, 'wdiGrowth', GroupVar = 'country', slideBy = -1,
              NewVar = 'growth_Lag1')
Comb <- slide(Comb, 'wdiGrowth', GroupVar = 'country', slideBy = -2,
              NewVar = 'growth_Lag2')

# Clean up
Comb <- DropNA(Comb, 'ContingentLiabilities')

#### Save
# write.dta(Comb, file = paste0(SD, 'BaseEUData_6May2014_v3.dta'))
