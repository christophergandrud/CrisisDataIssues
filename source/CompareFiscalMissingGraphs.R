################
# Compare countries with available data and those without
# Christopher Gandrud
# 14 February 2014
################

library(foreign)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(repmis)
library(psData)

Main <- source_data('https://raw2.github.com/christophergandrud/amcData/master/BaseFiles/LaeVal2012/LVCrisisResponseFull.csv',
                    stringsAsFactors = FALSE)

Sub <- Main[, c('Country', 'Start', 'FiscalCosts.PercGDP')]
Sub <- CountryID(Sub, countryVar = 'Country')
names(Sub) <- c('iso2c', 'country', 'year', 'LVFiscal')

# Create dummy of missing fiscal data
Sub$MissingFiscal <- 'Not Missing'
Sub$MissingFiscal[is.na(Sub$LVFiscal)] <- 'Missing'

# Polity IV data
PolityData <- PolityGet(vars = c('polity2'), OutCountryID = 'iso2c', duplicates = 'drop')
PolityData <- DropNA(PolityData, c('polity2'))
PolityData <- VarDrop(PolityData, 'country')

#### Database of Political Institutions data 
dpiVars <- c('eiec', 'checks', 'stabns')
DpiData <- DpiGet(vars = dpiVars, OutCountryID = 'iso2c', duplicates = 'drop')
DpiData[, dpiVars][DpiData[, dpiVars] == -999] <- NA

Comb <- dMerge(Sub, PolityData, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, DpiData, Var = c('iso2c', 'year'), all.x = TRUE)

#### Graph comparative densities ####
ggplot(Comb, aes(polity2, colour = as.factor(MissingFiscal), linetype = as.factor(MissingFiscal))) + 
  geom_density() + 
  scale_color_brewer(palette = 'Set1', name = 'LV Fiscal Data') +
  scale_linetype(name = 'LV Fiscal Data') +
  ylab('Density\n') + xlab('\nPolity IV') +
  theme_bw()

ggplot(Comb, aes(eiec, colour = as.factor(MissingFiscal), linetype = as.factor(MissingFiscal))) + 
  geom_density() + 
  scale_color_brewer(palette = 'Set1', name = 'LV Fiscal Data') +
  scale_linetype(name = 'LV Fiscal Data') +
  ylab('Density\n') + xlab('\nElectoral Cometitiveness') +
  theme_bw()

ggplot(Comb, aes(stabns, colour = as.factor(MissingFiscal), linetype = as.factor(MissingFiscal))) + 
  geom_density() + 
  scale_color_brewer(palette = 'Set1', name = 'LV Fiscal Data') +
  scale_linetype(name = 'LV Fiscal Data') +
  ylab('Density\n') + xlab('\nPolitical Stability') +
  theme_bw()

ggplot(Comb, aes(checks, colour = as.factor(MissingFiscal), linetype = as.factor(MissingFiscal))) + 
  geom_density() + 
  scale_color_brewer(palette = 'Set1', name = 'LV Fiscal Data') +
  scale_linetype(name = 'LV Fiscal Data') +
  ylab('Density\n') + xlab('\nChecks') +
  theme_bw()

