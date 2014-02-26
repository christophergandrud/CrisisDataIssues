################
# Keefer (2007) data extender
# Christopher Gandrud
# 26 February 2014
###############

# Load packages
library(DataCombine)
library(countrycode)
library(psData)
library(forecast)
library(plyr)
library(WDI)
library(foreign)

# Fuction for keefer rolling 3 year averages
rollmean3r <- function(x){
  x <- shift(x, -2, reminder = FALSE)
  ma(x, 3, centre = FALSE)
}

rollmean3f <- function(x){
  x <- shift(x, 2, reminder = FALSE)
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
Fiscal <- VarDrop(Fiscal, c('country', 'Notes'))
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
dpiVars <- c('eiec', 'checks', 'stabns', 'allhouse')
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

# Create Keefer political stability backwards lag
DpiData <- ddply(DpiData, .(country), transform, stabnsLag3 = rollmean3r(stabns))

# Find residuals for lagged check (modified from Keefer)
SubKeefer <- DropNA(DpiData, c('DiEiec33', 'Checks33'))
ResidKeefer <- lm(DiEiec33 ~ Checks33, data = SubKeefer)
SubKeefer$ChecksResiduals33 <- ResidKeefer$residuals
SubKeefer <- SubKeefer[, c('iso2c', 'year', 'ChecksResiduals33')]

# Create straight 3 year moving average lags and lead
## Lags
DpiData <- ddply(DpiData, .(country), transform, DiEiecLag3 = rollmean3r(DiEiec))
DpiData <- ddply(DpiData, .(country), transform, ChecksLag3 = rollmean3r(checks))
DpiData <- ddply(DpiData, .(country), transform, allhouseLag3 = rollmean3r(allhouse))

### Checks residuals 3 year lag
SubLag <- DropNA(DpiData, c('DiEiecLag3', 'allhouseLag3'))
ResidLag <- lm(DiEiecLag3 ~ allhouseLag3, data = SubLag)
SubLag$allhouseResidualsLag3 <- ResidLag$residuals
SubLag <- SubLag[, c('iso2c', 'year', 'allhouseResidualsLag3')]

### Allhouse residuals 3 year lag
SubLagAll <- DropNA(DpiData, c('DiEiecLag3', 'ChecksLag3'))
ResidLag <- lm(DiEiecLag3 ~ ChecksLag3, data = SubLagAll)
SubLagAll$ChecksResidualsLag3 <- ResidLag$residuals
SubLagAll <- SubLagAll[, c('iso2c', 'year', 'ChecksResidualsLag3')]

## Leads
DpiData <- ddply(DpiData, .(country), transform, DiEiecLead3 = rollmean3f(DiEiec))
DpiData <- ddply(DpiData, .(country), transform, ChecksLead3 = rollmean3f(checks))
DpiData <- ddply(DpiData, .(country), transform, stabnsLead3 = rollmean3f(stabns))
DpiData <- ddply(DpiData, .(country), transform, allhouseLead3 = rollmean3f(allhouse))

### Checks residuals 3 year lead
SubLead <- DropNA(DpiData, c('DiEiecLead3', 'ChecksLead3'))
ResidLead <- lm(DiEiecLead3 ~ ChecksLead3, data = SubLead)
SubLead$ChecksResidualsLead3 <- ResidLead$residuals
SubLead <- SubLead[, c('iso2c', 'year', 'ChecksResidualsLead3')]

### Allhouse residuals 3 year lead
SubLeadAll <- DropNA(DpiData, c('DiEiecLead3', 'allhouseLead3'))
ResidLeadAll <- lm(DiEiecLead3 ~ allhouseLead3, data = SubLeadAll)
SubLeadAll$allhouseResidualsLead3 <- ResidLeadAll$residuals
SubLeadAll <- SubLeadAll[, c('iso2c', 'year', 'allhouseResidualsLead3')]

### Create 3 year leads for time periods begining 2 years in the future
DpiData <- slideMA(DpiData, Var = 'DiEiec', GroupVar = 'country', periodBound = 5, offset = 2)
DpiData <- slideMA(DpiData, Var = 'checks', GroupVar = 'country', periodBound = 5, offset = 2)
DpiData <- slideMA(DpiData, Var = 'stabns', GroupVar = 'country', periodBound = 5, offset = 2)

### Checks residuals leads for time periods begining 2 years in the future
SubLead3 <- DropNA(DpiData, c('DiEiecMA5_2', 'checksMA5_2'))
ResidLead3 <- lm(DiEiecMA5_2 ~ checksMA5_2, data = SubLead3)
SubLead3$ChecksResidualsLead5_2 <- ResidLead3$residuals
SubLead3 <- SubLead3[, c('iso2c', 'year', 'ChecksResidualsLead5_2')]

##### Winset and selectorate data ####
Win <- WinsetCreator()
Win <- VarDrop(Win, 'country')

#### IMF program
IMF <- IMF_WBGet(sheets = c('IMF SBA 5', 'IMF EFF 5', 'IMF SAF 5'))
IMF <- DropNA(IMF, 'IMF.SBA.5')
IMF$IMFProgramAny <- 0
IMF$IMFProgramAny[IMF$IMF.SBA.5 == 1 | IMF$IMF.EFF.5 == 1 | IMF$IMF.SAF.5 == 1] <- 1
IMF <- IMF[order(IMF$country, IMF$year), ]

IMF <- slideMA(IMF, Var = 'IMFProgramAny', GroupVar = 'country', periodBound = 3, NewVar = 'IMFProgramLead3')
IMF <- VarDrop(IMF, 'country')

#### Economic Data from the World Bank Development Indicators
Countries <- unique(DpiData$iso2c)
Wdi <- WDI(country = Countries,
           indicator = c('NY.GDP.PCAP.PP.KD', 'NY.GDP.PCAP.KD.ZG', 'BN.CAB.XOKA.GD.ZS', 'BM.GSR.GNFS.CD', 
                         'BX.GSR.GNFS.CD', 'FI.RES.TOTL.DT.ZS', 'NY.GDP.MKTP.CD'),
           start = 1970, end = 2012)
names(Wdi) <- c('iso2c', 'country', 'year', 'GDPperCapita', 'GDPChange', 'CurrentAccount', 
                'Imports', 'Exports', 'Reserves', 'TotalGDP')
Wdi <- Wdi[order(Wdi$country, Wdi$year), ]

## Create transformed variables
# Income
Wdi <- ddply(Wdi, .(country), transform, Income33 = rollmean33(GDPperCapita))
Wdi <- slideMA(Wdi, Var = 'GDPperCapita', GroupVar = 'country', periodBound = -3, NewVar = 'IncomeLag3')
Wdi <- slideMA(Wdi, Var = 'GDPperCapita', GroupVar = 'country', periodBound = 3, NewVar = 'IncomeLead3')

# Growth
Wdi <- ddply(Wdi, .(country), transform, Growth33 = rollmean33(GDPChange))
Wdi <- slideMA(Wdi, Var = 'GDPChange', GroupVar = 'country', periodBound = 3, NewVar = 'GrowthLead3')

# Total GDP
Wdi <- slideMA(Wdi, Var = 'TotalGDP', GroupVar = 'country', periodBound = 3, NewVar = 'GDPLead3')

# CurrentAccount 1
Wdi <- slide(Wdi, Var = 'CurrentAccount', GroupVar = 'country', NewVar = 'CurrentAccountLag1')

# CurrentAccount 2
Wdi$CurrentAccountMinus <- Wdi$CurrentAccount - Wdi$CurrentAccountLag1 

# ChangeTermsTrade
Wdi$Terms <- Wdi$Exports/Wdi$Imports
Wdi <- PercChange(Wdi, Var = 'Terms', GroupVar = 'country', NewVar = 'TermsChange', type = 'proportion')

WdiSlim <- Wdi[, c('iso2c', 'year', 'GDPperCapita', 'Income33', 'IncomeLag3', 'IncomeLead3', 'Growth33', 'GrowthLead3', 
                   'CurrentAccountLag1', 'CurrentAccountMinus', 'TotalGDP', 'GDPLead3',
                   'TermsChange', 'Reserves')]

##### Combine data sets
Comb <- dMerge(DpiData, SubKeefer, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, SubLag, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, SubLagAll, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, SubLead, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, SubLead3, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, SubLeadAll, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, PolityData, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, IMF, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, Win, Var = c('iso2c', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, Fiscal, Var = c('iso2c', 'year'), all.y = TRUE)
Comb <- dMerge(Comb, WdiSlim, Var = c('iso2c', 'year'), all.x = TRUE)
Comb$country <- countrycode(Comb$iso2c, origin = 'iso2c', destination = 'country.name')


#### Revision Create errors variable
CombRevis = Comb
CombRevis$Diff_LVH <- (CombRevis$LV2012.Fiscal - CombRevis$Honohan2003.Fiscal)
CombRevis$Diff_LVC <- (CombRevis$LV2012.Fiscal - CombRevis$Caprio1996.Fiscal)
CombRevis$Diff_HC <- (CombRevis$Honohan2003.Fiscal - CombRevis$Caprio1996.Fiscal)

#### Create indicator for whether or not data was revised by Laeven and Valencia (2012)
CombRevis$Revision <- 0
CombRevis$Revision[(abs(CombRevis$Diff_LVH) > 0 & CombRevis$LV2012.Fiscal != CombRevis$Caprio1996.Fiscal)] <- 1
CombRevis$Revision[abs(CombRevis$Diff_LVC) > 0] <- 1

CombRevis <- NaVar(CombRevis, c('LV2012.Fiscal', 'Honohan2003.Fiscal', 'Caprio1996.Fiscal'))
CombRevis$Revision[(CombRevis$Miss_LV2012.Fiscal == 0 & CombRevis$Miss_Honohan2003.Fiscal == 1 & 
                     CombRevis$LV2012.Fiscal != CombRevis$Caprio1996.Fiscal)] <- 1
CombRevis$Revision[(CombRevis$Miss_LV2012.Fiscal == 0 & is.na(CombRevis$Caprio1996.Fiscal) & CombRevis$year < 1996)] <- 1
CombRevis$Revision[(CombRevis$Miss_LV2012.Fiscal %in% 0 & CombRevis$Miss_Caprio1996.Fiscal %in% 1 & 
                      CombRevis$year < 1996)] <- 1

# Recode Philipinnes as no change as change was caused by a coding error in Honohan & Klingebiel (2003)
CombRevis$Revision[CombRevis$iso2c %in% 'PH' & CombRevis$year %in% 1983] <- 0

# Merge years assuming that LV (2012) has correct start year
source('/git_repositories/CrisisDataIssues/source/RevisedRevision.R')

# Save to Stata format
write.dta(CombRevis, file = '/git_repositories/CrisisDataIssues/data/KeeferExtended.dta')

############################################################  
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

