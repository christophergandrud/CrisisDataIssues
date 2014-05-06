################
# Create basic EU data set for simple test regressions
# Christopher Gandrud
# 6 May 2014
###############

# Load packages
library(DataCombine)
library(WDI)
library(foreign)

# Set directories
# Set data directory
DD <- '/git_repositories/CrisisDataIssues/data/'

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

Comb <- DropNA(Comb, 'ContingentLiabilities')

Comb$CollapseElect <- Comb$yrcurnt
Comb$CollapseElect[Comb$yrcurnt > 1] <- 3
Comb$CollapseElect <- factor(Comb$CollapseElect, 
                             labels = c('Election Year', 'Elect. - 1', 
                                        'Other Years'))


# Remove Finland (only has one observation year)
Comb <- subset(Comb, country != 'FI')

# Remove Slovakia (only has two years)
Comb <- subset(Comb, country != 'SK')

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
NPL <- WDI(indicator = 'FB.AST.NPER.ZS', end = '2013')

NPL <- NPL[, c('iso2c', 'year', 'FB.AST.NPER.ZS')]

names(NPL) <- c('country', 'year', 'wdiNpl')

Comb <- dMerge(Comb, NPL, Var = c('country', 'year'), all.x = TRUE)
Comb <- Comb[order(Comb$country, Comb$year), ]

Comb <- slide(Comb, 'wdiNpl', GroupVar = 'country', slideBy = -1)
Comb <- slide(Comb, 'wdiNpl', GroupVar = 'country', slideBy = -2)
