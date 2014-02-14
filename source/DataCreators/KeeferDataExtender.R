################
# Keefer data extender
# Christopher Gandrud
# 14 February 2014
###############

library(DataCombine)
library(countrycode)
library(psData)
library(forecast)
library(plyr)
library(WDI)
library(foreign)

# Fuction for keefer rolling 3 year averages
rollmean3r <- function(x){
  x <- shift(x, -2)
  ma(x, 3, centre = FALSE)
}

rollmean3f <- function(x){
  x <- shift(x, 2)
  ma(x, 3, centre = FALSE)
}

rollmean33 <- function(x){
  xR <- rollmean3r(x)
  xF <- rollmean3f(x)
  Comb <- (xR + xF)/2
}

#### Fiscal transfers data (both Laeven and Valencia (2012) and Keefer (2007))
Fiscal <- read.csv('/git_repositories/CrisisDataIssues/data/KefferFiscal.csv', 
                   stringsAsFactors = FALSE)
Fiscal <- VarDrop(Fiscal, 'country')
Fiscal$HonohanCrisisOngoing[is.na(Fiscal$HonohanCrisisOngoing)] <- 0

# Include ongoing crisis variable
Ongoing <- read.csv('/git_repositories/amcData/BaseFiles/LaeVal2012/LVCrisisResponseFull.csv', 
                    stringsAsFactors = FALSE )

Ongoing <- CountryID(Ongoing, countryVar = 'Country')

Ongoing$ongoing <- 0
Ongoing$ongoing[Ongoing$End == 'ongoing'] <- 1
Ongoing <- Ongoing[, c('iso2c', 'Start', 'ongoing')]
names(Ongoing) <- c('iso2c', 'year', 'ongoingLV')

Fiscal <- dMerge(Fiscal, Ongoing, Var = c('iso2c', 'year'), all = TRUE)
Fiscal <- subset(Fiscal, iso2c != '') # Drop Czechoslovakia

# Polity IV data
PolityData <- PolityGet(vars = c('polity2'), OutCountryID = 'iso2c', duplicates = 'drop')
PolityData <- DropNA(PolityData, c('polity2'))
PolityData <- VarDrop(PolityData, 'country')

#### Database of Political Institutions data 
dpiVars <- c('eiec', 'checks', 'stabns')
DpiData <- DpiGet(vars = dpiVars, OutCountryID = 'iso2c', duplicates = 'drop')
DpiData[, dpiVars][DpiData[, dpiVars] == -999] <- NA
# DpiData <- DropNA(DpiData, c('eiec', 'checks'))

DpiData <- DpiData[order(DpiData$country, DpiData$year), ]

# Dichotomize electoral competitiveness
DpiData$DiEiec <- 0
DpiData$DiEiec[DpiData$eiec >= 6] <- 1

# Create Keefer Forward and Backward Lags
DpiData <- ddply(DpiData, .(country), transform, DiEiec33 = rollmean33(DiEiec))
DpiData <- ddply(DpiData, .(country), transform, Checks33 = rollmean33(checks))

# Create backwards lags
DpiData <- ddply(DpiData, .(country), transform, stabnsLag3 = rollmean3r(stabns))

# Find residuals for lagged check
Sub <- DropNA(DpiData, c('DiEiec33', 'Checks33'))
Resid <- lm(DiEiec33 ~ Checks33, data = Sub)
Sub$ChecksResiduals33 <- Resid$residuals
Sub <- Sub[, c('iso2c', 'year', 'ChecksResiduals33')]

# Winset and selectorate data
Win <- WinsetCreator()
Win <- VarDrop(Win, 'country')

#### Economic Data from the World Bank Development Indicators
Countries <- unique(DpiData$iso2c)
Wdi <- WDI(country = Countries,
           indicator = c('NY.GDP.PCAP.PP.KD', 'NY.GDP.PCAP.KD.ZG', 'BN.CAB.XOKA.GD.ZS', 'BM.GSR.GNFS.CD', 
                         'BX.GSR.GNFS.CD', 'FI.RES.TOTL.DT.ZS'),
           start = 1970, end = 2012)
names(Wdi) <- c('iso2c', 'country', 'year', 'GDPperCapita', 'GDPChange', 'CurrentAccount', 
                'Imports', 'Exports', 'Reserves')
Wdi <- Wdi[order(Wdi$country, Wdi$year), ]

## Create transformed variables
# Income
Wdi <- ddply(Wdi, .(country), transform, Income33 = rollmean33(GDPperCapita))
# Growth
Wdi <- ddply(Wdi, .(country), transform, Growth33 = rollmean3r(GDPChange))
# CurrentAccount 1
Wdi <- slide(Wdi, Var = 'CurrentAccount', GroupVar = 'country', NewVar = 'CurrentAccountLag1')
# CurrentAccount 2
Wdi$CurrentAccountMinus <- Wdi$CurrentAccount - Wdi$CurrentAccountLag1 
# ChangeTermsTrade
Wdi$Terms <- Wdi$Exports/Wdi$Imports
Wdi <- PercChange(Wdi, Var = 'Terms', GroupVar = 'country', NewVar = 'TermsChange', type = 'proportion')

WdiSlim <- Wdi[, c('iso2c', 'year', 'GDPperCapita', 'Income33', 'Growth33', 'CurrentAccountLag1', 'CurrentAccountMinus',
                   'TermsChange', 'Reserves')]

##### Combine data sets
Comb <- dMerge(DpiData, Sub, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, PolityData, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, Win, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, Fiscal, Var = c('iso2c', 'year'), all.y = TRUE)
Comb <- dMerge(Comb, WdiSlim, Var = c('iso2c', 'year'), all.x = TRUE)
Comb$country <- countrycode(Comb$iso2c, origin = 'iso2c', destination = 'country.name')

write.dta(Comb, file = '/git_repositories/CrisisDataIssues/data/KeeferExtended.dta')

##### Create Reinhart and Rogoff (2010) combination #####

## Download RR crisis data
RR <- RRCrisisGet()
rr <- RR

write.csv(rr, file = '/git_repositories/CrisisDataIssues/data/ReinhartRogoffCrisis.csv', row.names = FALSE)

# Keep if independent
rr <- subset(rr, RR_Independence == 1)

# Keep if first year of crisis
rr <- rr[order(rr$country, rr$year),]
rr <- slide(rr, Var = 'RR_BankingCrisis', GroupVar = 'country', slideBy = 1)

# Merge
CombRR <- dMerge(rr, Fiscal, Var = c('iso2c', 'year'), all.x = TRUE)
CombRR <- dMerge(CombRR, PolityData, Var = c('iso2c', 'year'), all.x = TRUE)

write.csv(CombRR, file = '/git_repositories/CrisisDataIssues/data/ReinhartRogoffFiscalPolity.csv', row.names = FALSE)

#### Reinhart and Rogoff/Laeven and Valencia Start and Stop

# Load LV data
LVFull <- read.csv('/git_repositories/amcData/BaseFiles/LaeVal2012/LVCrisisResponseFull.csv', 
                    stringsAsFactors = FALSE )

# Keep country, start, and stop years
LVSub <- LVFull[, 1:3]

# Replace ongoing with 2011
LVSub$End[LVSub$End == 'ongoing'] <- '2011'

LVNew <- TimeFill(LVSub, GroupVar = 'Country', StartVar = 'Start', EndVar = 'End', 
                  NewVar = 'LV_SystemCrisis', NewTimeVar = 'year')

LVNew$Country <- as.character(LVNew$Country)

LVNew <- CountryID(LVNew, countryVar = 'Country')
LVNew <- LVNew[, c('iso2c', 'year', 'LV_SystemCrisis')]

CombRRLV <- dMerge(rr, LVNew, Var = c('iso2c', 'year'), all = TRUE)

write.csv(CombRRLV, file = '/git_repositories/CrisisDataIssues/data/ReinhartRogoffLVCount.csv', row.names = FALSE)

