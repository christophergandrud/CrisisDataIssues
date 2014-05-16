###########
# Replication file for 'How they spend it'
# Christopher Gandrud
# 6 May 2014
###########

# Set working directory
WD <- '~/Dropbox/AMCProject/CrisisDataIssuesPaper Keefer/figures/'
setwd(WD)

# Set data directory
DD <- '/git_repositories/CrisisDataIssues/data/'

# Load packages
library(foreign)
library(reshape2)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(DataCombine)
library(psData)
library(countrycode)

# -------------------------------------------------------------------- #
#### Compare fiscal costs in LV vs. HK ####
## Data set created using:
## https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/KeeferDataExtender.R

Main <- read.dta(paste0(DD, 'KeeferExtended.dta'))

Main$Diff <- Main$LV2012_Fiscal - Main$Honohan2003_Fiscal
cor.test(Main$LV2012_Fiscal, Main$Honohan2003_Fiscal)

Main$HKOngoing[Main$HonohanCrisisOngoing == 0] <- 'Crisis Complete'
Main$HKOngoing[Main$HonohanCrisisOngoing == 1] <- 'Crisis Ongoing'
Main$HKOngoing[(Main$iso2c %in% 'PH' & Main$year %in% 1983)] <- 'Likely Coding Error'

eLabels <- c('Low Comp.', 'High Comp.')
Main$DiEiecL <- factor(Main$DiEiec, levels = c(0, 1), labels = eLabels)
Main$DiEiecL <- relevel(Main$DiEiecL, ref = 'High Comp.')


PlotDiff <- ggplot(Main, aes(year, Diff, colour = HKOngoing, label = iso2c,
    shape = DiEiecL)) +
    geom_jitter(position = position_jitter(width = .5, height = 0), size = 3) +
    geom_text(angle = 30, vjust = -1) +
    scale_x_continuous(limits = c(1975, 2000)) +
    scale_colour_manual(values = c('black', 'grey', 'red'), name = '') +
    scale_shape(name = 'Electoral\nCompetitiveness') +
    geom_hline(aes(yintercept = 0), linetype = 'dotted') +
    xlab('') + ylab('Laeven & Valencia - Honohan & Klingebiel\n') +
    theme_linedraw(base_size = 15)

pdf('FiscalDifference.pdf', width = 10)
    PlotDiff
dev.off()


# -------------------------------------------------------------------- #
#### Compare cost densities ####
MainSub2000 <- subset(Main, year <= 2000)
MainSub2_HK <- subset(MainSub2000, !is.na(Honohan2003_Fiscal))

P1 <- ggplot(MainSub2000, aes(Honohan2003_Fiscal, colour = DiEiecL,
        linetype = DiEiecL)) + geom_density(size = 1) +
        scale_color_brewer(palette = 'Set1', guide = FALSE) +
        scale_y_continuous(breaks = c(0, 0.025, 0.05)) +
        #scale_color_brewer(palette = 'Set1', name = 'Electoral\nCompetitiveness') +
        scale_linetype_discrete(guide = FALSE) +
        #scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('') + xlab('') + ggtitle('Honohan & Klingebiel (2003)') +
        theme_linedraw()

P2 <- ggplot(MainSub2_HK, aes(LV2012_Fiscal, colour = DiEiecL,
        linetype = DiEiecL)) + geom_density(size = 1) +
        scale_color_brewer(palette = 'Set1',
        name = 'Electoral\nCompetitiveness') +
        scale_y_continuous(breaks = c(0, 0.025, 0.04)) +
        scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('Denisty\n') + xlab('') +
        ggtitle('Laeven and Valencia (2012) in Honohan & Klingebiel (2003)') +
        theme_linedraw()

P3 <- ggplot(Main, aes(LV2012_Fiscal, colour = DiEiecL, linetype = DiEiecL)) +
        geom_density(size = 1) +
        scale_color_brewer(palette = 'Set1', guide = FALSE) +
        scale_y_continuous(breaks = c(0, 0.025, 0.05)) +
        scale_linetype_discrete(guide = FALSE) +
        ylab('') + xlab('Fiscal Costs (% GDP)') +
        ggtitle('Laeven and Valencia (2012) before 2001') +
        theme_linedraw()

# Combine
gP1 <- ggplotGrob(P1)
gP2 <- ggplotGrob(P2)
gP3 <- ggplotGrob(P3)

maxWidth = grid::unit.pmax(gP1$widths[2:5], gP2$widths[2:5], gP3$widths[2:5])
gP1$widths[2:5] <- as.list(maxWidth)
gP2$widths[2:5] <- as.list(maxWidth)
gP3$widths[2:5] <- as.list(maxWidth)

pdf('LV_HK_CompareElect.pdf', width = 10)
        grid.arrange(arrangeGrob(gP1, gP2, gP3, ncol = 1, heights = c(3, 3, 3)))
dev.off()


# -------------------------------------------------------------------- #
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

Comb <- subset(Comb, year != 2013) ## 5 years from the start of the crisis
Comb <- subset(Comb, year >= 2009) ## Only UK has data in 2007
Comb <- subset(Comb, yrcurnt !=5) ## Only one observation

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

#### Contingent liability plots ####
CLPlot1 <- ggplot(Comb, aes(ContingentLiabilities_Standard)) + 
    geom_density(color = '#a6bddb', fill = '#a6bddb', alpha = 0.7) + 
    facet_grid(CollapseElect ~ .) +
    scale_y_continuous(breaks = c(0, 0.3)) +
    geom_vline(xintercept = 0) +
    xlab('') + ylab('Density\n') +
    theme_bw(base_size = 12)

CLPlot2 <- ggplot(Comb, aes(ContingentLiabilities_Standard)) + 
    geom_density(color = '#a6bddb', fill = '#a6bddb', alpha = 0.7) +
    facet_grid(year ~ .) +
    scale_y_continuous(breaks = c(0, 0.3)) +
    geom_vline(xintercept = 0) +
    xlab('\n Contingent liabilities (St. dev. from country mean)') +
    ylab('Density\n') +
    theme_bw(base_size = 12)

pdf('CL_Plot.pdf', width = 10)
    grid.arrange(CLPlot1, CLPlot2, nrow = 2)
dev.off()

#### Realized liability plots ####
RLPlot1 <- ggplot(Comb, aes(GovLiabilities_Standard)) + 
    geom_density(color = '#a6bddb', fill = '#a6bddb', alpha = 0.7) +
    facet_grid(CollapseElect ~ .) +
    scale_y_continuous(breaks = c(0, 0.3)) +
    geom_vline(xintercept = 0) +
    xlab('') +
    ylab('Density\n') +
    theme_bw(base_size = 12)

RLPlot2 <- ggplot(Comb, aes(GovLiabilities_Standard)) + 
    geom_density(color = '#a6bddb', fill = '#a6bddb', alpha = 0.7) + 
    facet_grid(year ~ .) +
    scale_y_continuous(breaks = c(0, 0.3)) +
    geom_vline(xintercept = 0) +
    xlab('\n Realized liabilities (St. dev. from country mean)') +
    ylab('Density\n') +
    theme_bw(base_size = 12)

pdf('Liabilities.pdf', width = 10)
    grid.arrange(RLPlot1, RLPlot2, nrow = 2)
dev.off()