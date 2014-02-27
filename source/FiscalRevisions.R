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
MNon3 <- zelig(YearMergeRevise ~ checks + DiEiec, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MNon4 <- zelig(YearMergeRevise ~ allhouse + DiEiec, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MNon5 <- zelig(YearMergeRevise ~ checks + DiEiec + stabns, data = Main, model = 'logit',
               method = 'weave', cite = FALSE)
MNon6 <- zelig(YearMergeRevise ~ checks + DiEiec + log(GDPperCapita), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')

# Three years before
MLag1 <- zelig(YearMergeRevise ~ ChecksLag3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag2 <- zelig(YearMergeRevise ~ allhouseLag3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag3 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag4 <- zelig(YearMergeRevise ~ allhouseResidualsLag3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag5 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 + DiEiecLag3, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag6 <- zelig(YearMergeRevise ~ allhouseResidualsLag3 + DiEiecLag3, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLag7 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 + DiEiecLag3 + stabnsLag3, data = Main, model = 'logit',
               method = 'weave', cite = FALSE)
MLag8 <- zelig(YearMergeRevise ~ log(IncomeLag3), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')
MLag9 <- zelig(YearMergeRevise ~ ChecksResidualsLag3 + DiEiecLag3 + stabnsLag3 + log(IncomeLag3), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')

# Three years after
MLead1 <- zelig(YearMergeRevise ~ ChecksLead3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLead2 <- zelig(YearMergeRevise ~ allhouseLead3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLead3 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLead4 <- zelig(YearMergeRevise ~ allhouseResidualsLead3 , data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLead5 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLead6 <- zelig(YearMergeRevise ~ allhouseResidualsLead3 + DiEiecLead3, data = Main, model = 'logit', cite = FALSE, 
               method = 'weave')
MLead7 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3, data = Main, model = 'logit',
               method = 'weave', cite = FALSE)
MLead8 <- zelig(YearMergeRevise ~ log(IncomeLead3), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')
MLead9 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3 + log(IncomeLead3), data = Main, 
               model = 'logit', cite = FALSE, method = 'weave')
MLead10 <- zelig(YearMergeRevise ~ ChecksResidualsLead3 + DiEiecLead3 + stabnsLead3 + log(IncomeLead3) + IMFProgramLead3, data = Main, 
                model = 'logit', cite = FALSE, method = 'weave')

Main$LogIncome <- log(Main$IncomeLead3)
Main$LogIncomePoly2 <- Main$LogIncome^2

MainNoNA <- DropNA(Main, c('YearMergeRevise', 'LogIncome', 'LogIncomePoly2'))

MLead11 <- zelig(YearMergeRevise ~ LogIncome + LogIncomePoly2, data = Main, 
                 model = 'logit', cite = FALSE, method = 'weave')

MLead11 <- zelig(YearMergeRevise ~ LogIncome, data = Main, 
                 model = 'logit', cite = FALSE, method = 'weave')

Mset <- setx(MLead11, LogIncome = 5:11)
Msim <- sim(MLead11, Mset)
plot.ci(Msim)

MLead11 <- zelig(YearMergeRevise ~ AMCAnyLead3 + LogIncome + LogIncomePoly2, data = Main, 
                 model = 'logit', cite = FALSE, method = 'weave')


MainSub <- DropNA(Main, 'LV2012_Fiscal')

MLead11 <- zelig(YearMergeRevise ~ LogIncome, data = MainSub, 
                 model = 'logit', cite = FALSE, method = 'weave')


# Simple logistic regression (Remove Thailand outlier)
MainTHSub <- Main[!(Main$iso2c %in% 'TH' & Main$year %in% 1997), ]
M_TH <- zelig(YearMergeRevise ~ ChecksResiduals33 + DiEiec33, data = MainTHSub, model = 'logit', cite = FALSE, 
              method = 'weave')

Main$FDI_GDP <- Main$FDILead3/Main$GDPLead3
Main$FDIMillionsLead3 <- Main$FDILead3/1000000

Main$FDIRescale <- log(Main$FDIMillionsLead3 + (abs(min(Main$FDIMillionsLead3, na.rm = TRUE)) + 0.1))
MLead11 <- zelig(YearMergeRevise ~ FDIRescale + LogIncome, data = Main, 
                 model = 'logit', cite = FALSE, method = 'weave')

MLead11 <- glm(YearMergeRevise ~ FDIRescale + LogIncome, data = Main, 
                 family = 'binomial')

MainSub <- Main[-73, ]
