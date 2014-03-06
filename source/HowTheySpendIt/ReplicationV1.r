###########
# Replication file for 'How they spend it'
# Christopher Gandrud
# 6 March 2014
###########
 
# Set working directory
setwd('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/')

# Load packages
library(foreign)
library(reshape2)
library(ggplot2)
library(gridExtra)


#### Compare fiscal costs in LV vs. HK ####
## Data set created using:
## https://github.com/christophergandrud/CrisisDataIssues/blob/master/source/DataCreators/KeeferDataExtender.R

Main <- read.dta('/git_repositories/CrisisDataIssues/data/KeeferExtended.dta')

Main$Diff <- Main$LV2012_Fiscal - Main$Honohan2003_Fiscal
cor.test(Main$LV2012_Fiscal, Main$Honohan2003_Fiscal)

Main$HKOngoing[Main$HonohanCrisisOngoing == 0] <- 'Crisis Complete'
Main$HKOngoing[Main$HonohanCrisisOngoing == 1] <- 'Crisis Ongoing'

PlotDiff <- ggplot(Main, aes(year, Diff, colour = HKOngoing, label = iso2c)) + 
  geom_jitter(position = position_jitter(width = .5, height = 0), size = 3) +
  geom_text(angle = 30, vjust = -1) +
  scale_x_continuous(limits = c(1975, 2000)) +
  scale_colour_manual(values = c('black', 'grey'), name = '') +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  xlab('') + ylab('Laeven & Valencia - Honohan & Klingebiel\n') +
  theme_bw(base_size = 15)

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/figures/FiscalDifference.pdf', width = 10)
  PlotDiff
dev.off()


#### Compare cost densities ####
eLabels <- c('Low Comp.', 'High Comp.')
Main$DiEiecL <- factor(Main$DiEiec, levels = c(0, 1), labels = eLabels)

MainSub2000 <- subset(Main, year <= 2000)

P1 <- ggplot(MainSub2000, aes(Honohan2003_Fiscal, colour = DiEiecL, linetype = DiEiecL)) + geom_density() + 
        scale_color_brewer(palette = 'Set1', name = 'Electoral\nCompetitiveness') +
        scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('Denisty\n') + xlab('Fiscal Costs (% GDP)') + ggtitle('Honohan & Klingebiel (2003)') +
        theme_bw()

P2 <- ggplot(Main, aes(LV2012_Fiscal, colour = DiEiecL, linetype = DiEiecL)) + geom_density() + 
        scale_color_brewer(palette = 'Set1', name = 'Electoral\nCompetitiveness') +
        scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('Denisty\n') + xlab('Fiscal Costs (% GDP)') + ggtitle('Laeven and Valencia (2012)') +
        theme_bw()

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/figures/LV_HK_CompareElect.pdf', width = 10)
  grid.arrange(P1, P2)
dev.off()
