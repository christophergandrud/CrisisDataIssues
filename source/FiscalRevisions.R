##############
# Analysis of the causes of fiscal revisions
# Christopher Gandrud
# 26 February 2014
##############

# Load packages
library(foreign)
library(DataCombine)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(Zelig)

# Load data
Main <- read.dta('/git_repositories/CrisisDataIssues/data/KeeferExtended.dta')

### Examine magnitude of revisions (excludes revisions that go from NA to a number)
ErrorVars <- c('Diff_LVH', 'Diff_LVC', 'Diff_HC')
for (i in ErrorVars){
  Main[, paste0('SD_', i)] <- Main[, i]/sd(Main[, i], na.rm = TRUE)
}

# Graph of how many standard deviations away an observations errors are
ggplot(Main, aes(DiEiec33, SD_Diff_LVH)) + geom_jitter() + stat_smooth(se = FALSE) + theme_bw()

ggplot(Main, aes(Checks33, SD_Diff_LVH)) + geom_jitter() + stat_smooth(se = FALSE) + theme_bw()

ggplot(Main, aes(log(Income33), SD_Diff_LVH)) + geom_jitter() + stat_smooth(se = FALSE) + theme_bw()

#### Examine if any revisions were made
Main = Main[Main$year <= 2000, ] # pre-Laeven and Valencia data only goes to 2000 

Main <- DropNA(Main, 'YearMergeRevise')

# ggplot(Main, aes(log(GDPperCapita), colour = as.factor(YearMergeRevise))) + geom_density() + theme_bw()
# ggplot(Main, aes(log(IncomeLead3), colour = as.factor(YearMergeRevise))) + geom_density() + theme_bw()
# ggplot(Main, aes(DiEiecLead3, colour = as.factor(YearMergeRevise))) + geom_density() + theme_bw()


# Simple logistic regression using Keefer Lags
M1 <- zelig(YearMergeRevise ~ ChecksResiduals33 , data = Main, model = 'logit', cite = FALSE, 
            method = 'weave')
M2 <- zelig(YearMergeRevise ~ ChecksResiduals33 + DiEiec33, data = Main, model = 'logit', cite = FALSE, 
            method = 'weave')
M3 <- zelig(YearMergeRevise ~ ChecksResiduals33 + DiEiec33 + stabnsLag3, data = Main, model = 'logit', cite = FALSE, 
            method = 'weave')
M4 <- zelig(YearMergeRevise ~ ChecksResiduals33 + DiEiec33 + log(GDPperCapita), data = Main, model = 'logit', cite = FALSE, 
            method = 'weave')

# Crisis year (no lags)
MNon1 <- zelig(YearMergeRevise ~ checks , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MNon2 <- zelig(YearMergeRevise ~ allhouse , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MNon3 <- zelig(YearMergeRevise ~ checks + eiec, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MNon4 <- zelig(YearMergeRevise ~ checks + eiec + stabns, data = Main, model = 'logit',
               method = 'weave', cite = FALSE)
MNon5 <- zelig(YearMergeRevise ~ checks + eiec + log(GDPperCapita), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')

# Three years before
MLag1 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag2 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 + DiEiecLag3, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag3 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 + DiEiecLag3 + stabnsLag3, data = Main, model = 'logit',
               method = 'weave', cite = FALSE)
MLag4 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 + DiEiecLag3 + log(GDPperCapita), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')

# Three years after
MLead1 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 , data = Main, model = 'logit', cite = FALSE, 
                method = 'weave')
MLead1 <- zelig(YearMergeRevise ~ ChecksLead3 , data = Main, model = 'logit', cite = FALSE, 
                method = 'weave')
MLead2 <- zelig(YearMergeRevise ~ allhouseLead3, data = Main, model = 'logit', cite = FALSE, 
                method = 'weave')

MLead2 <- zelig(YearMergeRevise ~ allhouseLead3 + log(IncomeLead3), data = Main, model = 'logit', cite = FALSE, 
                method = 'weave')

MLead3 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3, data = Main, model = 'logit', 
                cite = FALSE, method = 'weave')
MLead4 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3, data = Main, 
                model = 'logit', cite = FALSE, method = 'weave')
MLead5 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3 + IncomeLead3, data = Main, 
               model = 'logit', method = 'weave', cite = FALSE)
MLead6 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3 + log(IncomeLead3), data = Main, 
                model = 'logit', method = 'weave', cite = FALSE)
MLead7 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3 + log(GDPLead3), data = Main, 
                model = 'logit', method = 'weave', cite = FALSE)


# Simple logistic regression (Remove Thailand outlier)
MainTHSub <- Main[!(Main$iso2c %in% 'TH' & Main$year %in% 1997), ]
M_TH <- zelig(YearMergeRevise ~ ChecksResiduals33 + DiEiec33, data = MainTHSub, model = 'logit', cite = FALSE, 
              method = 'weave')
