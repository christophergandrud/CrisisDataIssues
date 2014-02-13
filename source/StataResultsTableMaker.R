###############
# Create Keefer Replication Results Table
# Christopher Gandrud
# 10 February 2014
###############

library(foreign)
library(xtable)

setwd('~/Dropbox/AMCProject/CrisisDataIssuesPaper/tables/')

KeefOrig <- read.dta('KeeferBasic.dta')
Hono <- read.dta('Honohan.dta')
LVKeefer <- read.dta('LVPre2001KeeferSample.dta')
LVSub <- read.dta('LVPre2001.dta')
LVNoOnFull <- read.dta('LVNoOn.dta')
LVFull <- read.dta('LVFullBasic.dta')
KeefLog <- read.dta('KeeferLog.dta')
LVLog <- read.dta('LVFullLogged.dta')

Comb <- merge(KeefOrig, Hono, by = 'var', all = TRUE, sort = FALSE)
Comb <- merge(Comb, LVKeefer, by = 'var', all = TRUE, sort = FALSE)
Comb <- merge(Comb, LVSub, by = 'var', all = TRUE, sort = FALSE)
Comb <- merge(Comb, LVNoOnFull, by = 'var', all = TRUE, sort = FALSE)
Comb <- merge(Comb, LVFull, by = 'var', all = TRUE, sort = FALSE)
Comb <- merge(Comb, KeefLog, by = 'var', all = TRUE, sort = FALSE)
Comb <- merge(Comb, LVLog, by = 'var', all = TRUE, sort = FALSE)

# Clean up
Comb <- data.frame(Comb[c(1:6, 34, 35, 7:11), ])

Comb$var <- gsub(pattern = "^.*?_stderr", replacement = "", Comb$var)
Comb$var <- gsub(pattern = "_coef", replacement = "", Comb$var)

Comb$var <- gsub(pattern = "_cons", replacement = "Constant", Comb$var)
Comb$var <- gsub(pattern = "_con", replacement = "", Comb$var)

Comb$var <- gsub(pattern = "Checks33", replacement = "Checks_Residual_33", Comb$var)
Comb$var <- gsub(pattern = "DiEiec33", replacement = "Electoral Comp._33", Comb$var)
Comb$var <- gsub(pattern = "stabnsLag3", replacement = "Instability_AVG_LAG3", Comb$var)
Comb$var <- gsub(pattern = "ongoingLV", replacement = "Crisis Ongoing LV ", Comb$var)

Comb$var <- gsub(pattern = "r2", replacement = "R2", Comb$var)
Comb$var <- gsub(pattern = "N_clust", replacement = "No. of Clusters", Comb$var)

Temp <- c("KeeferOriginal", 'Honohan', "LVPre2001Keef", 'LVPre2001', 'LVNoOnFull', "LVFull", 'KeeferLog', 'LVFullLogged')
Comb[, Temp][is.na(Comb[, Temp])] <- ""

names(Comb) <- c('', 'Keefer', 'HK', 'LV-Keefer', 'LV pre-2001', 'LV Full', 'LV Full', 'Keefer Log', 'LV Full Log')

print(xtable(Comb, dcolumn = TRUE, booktabs = TRUE), size = 'scriptsize', include.rownames = FALSE, floating = FALSE,
      file = '~/Dropbox/AMCProject/CrisisDataIssuesPaper/tables/KeeferRepTable.tex')
