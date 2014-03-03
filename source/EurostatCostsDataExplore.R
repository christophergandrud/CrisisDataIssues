#################
# Eurostat crisis costs data explore (http://epp.eurostat.ec.europa.eu/portal/page/portal/government_finance_statistics/excessive_deficit/supplementary_tables_financial_turmoil)
# Christopher Gandrud
# 3 March 2014
#################

library(countrycode)
library(DataCombine)
library(plyr)
library(ggplot2)

# Load Eurostat financial crisis costs summary
EUCosts <- read.csv('/git_repositories/CrisisDataIssues/data/Eurostat_CrisisCosts.csv', stringsAsFactors = FALSE)

names(EUCosts) <- c("country", "year", "t", "NetCost", "GovAssets", "GovLiabilities", "ContingentLiabilities")

# Load Eurostat GDP data
GDP <- read.csv('/git_repositories/CrisisDataIssues/data/other/nama_gdp_c_1_Data.csv', stringsAsFactors = FALSE)

GDP <- GDP[, c(2, 1, 5)]
GDP$Value <- gsub(',', '', GDP$Value)
GDP$gdp <- as.numeric(GDP$Value)
GDP$country <- countrycode(GDP$GEO, origin = 'country.name', destination = 'iso2c')
GDP$country[GDP$country == 'GB'] <- 'UK'
GDP <- GDP[, c('country', 'TIME', 'gdp')]
names(GDP) <- c('country', 'year', 'gdp')

# Merge
Comb <- dMerge(EUCosts, GDP, Var = c('country', 'year'), all.x = TRUE)

# Create X/GDP
xgdp <- function(data, vars){
  for (i in vars){
    data[, paste0(i, 'PerGdp')] <- (data[, i]/data[, 'gdp']) * 100
  }
  return(data)
}

VarNames <- c("NetCost", "GovAssets", "GovLiabilities", "ContingentLiabilities")
Comb <- xgdp(data = Comb, vars = VarNames)
Comb <- DropNA(Comb, 't')

# Create cumulative costs 
Comb <- ddply(Comb, .(country), mutate, CumSumNetCosts = cumsum(NetCost))

# Create 'bagehot' index Assets/Contingent Liabilities
Comb$Bagehot <-  (Comb$GovAssets/Comb$ContingentLiabilities)

#### Plot ####
# Net costs/gdp
ggplot(Comb, aes(t, NetCostPerGdp)) + geom_line() + facet_grid(. ~ country) + theme_bw()

# Cumulative costs
ggplot(Comb, aes(t, CumSumNetCosts)) + geom_line() + 
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  ylab('Cumulative Costs (millions of Euros)\n') + xlab('\nYears from first year of Gov. Response') +
  facet_grid(. ~ country) + theme_bw()

# Bagehot index
ggplot(Comb, aes(t, Bagehot)) + geom_line() + 
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  ylab('Bagehot Index\n') + xlab('\nYears from first year of Gov. Response') +
  facet_grid(. ~ country) + theme_bw()

