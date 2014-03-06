#################
# Eurostat crisis costs data explore (http://epp.eurostat.ec.europa.eu/portal/page/portal/government_finance_statistics/excessive_deficit/supplementary_tables_financial_turmoil)
# Christopher Gandrud
# 3 March 2014
#################

library(countrycode)
library(DataCombine)
library(plyr)
library(psData)
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

# Years left in current term
YearsLeft <- DpiGet(vars = c('yrcurnt', 'stabns'))
YearsLeft$iso2c[YearsLeft$iso2c == 'GB'] <- 'UK'
YearsLeft <- YearsLeft[, -2]
names(YearsLeft) <- c('country', 'year', 'yrcurnt', 'stanbs')
YearsLeft$DiElection[YearsLeft$yrcurnt == 0] <- "Election Year"
YearsLeft$DiElection[YearsLeft$yrcurnt > 0] <- "Not Election Year"

# Merge
Comb <- dMerge(EUCosts, GDP, Var = c('country', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, YearsLeft, Var = c('country', 'year'), all.x = TRUE)


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
Comb$Bagehot[Comb$Bagehot == Inf] <- NA

Comb$Cont_to_Liabilities <-  (Comb$ContingentLiabilities/Comb$GovLiabilities)
Comb$Cont_to_Liabilities[Comb$Cont_to_Liabilities == Inf] <- NA


# Log Bagehot
Comb$LogBagehot <- log(Comb$Bagehot + (min(Comb$Bagehot, na.rm = TRUE) + 0.1))


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
  geom_hline(aes(yintercept = 1), linetype = 'dotted') +
  ylab('Bagehot Index\n') + xlab('\nYears from first year of Gov. Response') +
  facet_grid(. ~ country) + theme_bw()

CombSubE <- DropNA(Comb, 'DiElection')
ggplot(CombSubE, aes(DiElection, LogBagehot, label = country)) + geom_boxplot() + geom_text() +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  xlab('') + ylab('Log Bagehot Index\n') + theme_bw()

ggplot(Comb, aes(yrcurnt, LogBagehot)) + geom_point() + facet_grid(. ~ country) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  stat_smooth(method = 'lm', se = FALSE) + 
  scale_x_reverse() + xlab('\nYears to Election') +
  theme_bw()

ggplot(Comb, aes(yrcurnt, ContingentLiabilities)) + geom_jitter() + facet_grid(. ~ country) +
  stat_smooth(method = 'lm', se = FALSE) + 
  scale_x_reverse() + xlab('\nYears to Election') +
  theme_bw()

ggplot(Comb, aes(yrcurnt, GovLiabilities)) + geom_jitter() + facet_grid(. ~ country) +
  stat_smooth(method = 'lm', se = FALSE) + 
  scale_x_reverse() + xlab('\nYears to Election') +
  theme_bw()

ggplot(Comb, aes(yrcurnt, GovAssets)) + geom_jitter() + facet_grid(. ~ country) +
  stat_smooth(method = 'lm', se = FALSE) + 
  scale_x_reverse() + xlab('\nYears to Election') +
  theme_bw()

ggplot(Comb, aes(yrcurnt, NetCostPerGdp, colour = stanbs)) + geom_point() + facet_grid(. ~ country) +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  stat_smooth(method = 'lm', se = FALSE) + 
  scale_x_reverse() + xlab('\nYears to Election') +
  theme_bw()


  