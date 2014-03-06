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
Main$HKOngoing[(Main$iso2c %in% 'PH' & Main$year %in% 1983)] <- 'Likely Coding Error'

eLabels <- c('Low Comp.', 'High Comp.')
Main$DiEiecL <- factor(Main$DiEiec, levels = c(0, 1), labels = eLabels)
Main$DiEiecL <- relevel(Main$DiEiecL, ref = 'High Comp.')


PlotDiff <- ggplot(Main, aes(year, Diff, colour = HKOngoing, label = iso2c, shape = DiEiecL)) + 
  geom_jitter(position = position_jitter(width = .5, height = 0), size = 3) +
  geom_text(angle = 30, vjust = -1) +
  scale_x_continuous(limits = c(1975, 2000)) +
  scale_colour_manual(values = c('black', 'grey', 'red'), name = '') +
  scale_shape(name = 'Electoral\nCompetitiveness') +
  geom_hline(aes(yintercept = 0), linetype = 'dotted') +
  xlab('') + ylab('Laeven & Valencia - Honohan & Klingebiel\n') +
  theme_bw(base_size = 15)

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/figures/FiscalDifference.pdf', width = 10)
  PlotDiff
dev.off()


#### Compare cost densities ####
MainSub2000 <- subset(Main, year <= 2000)
MainSub2_HK <- subset(MainSub2000, !is.na(Honohan2003_Fiscal))

P1 <- ggplot(MainSub2000, aes(Honohan2003_Fiscal, colour = DiEiecL, linetype = DiEiecL)) + geom_density(size = 1) + 
        scale_color_brewer(palette = 'Set1', guide = FALSE) +
        scale_y_continuous(breaks = c(0, 0.025, 0.05)) +
        #scale_color_brewer(palette = 'Set1', name = 'Electoral\nCompetitiveness') +
        scale_linetype_discrete(guide = FALSE) +
        #scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('') + xlab('') + ggtitle('Honohan & Klingebiel (2003)') +
        theme_bw()

P2 <- ggplot(MainSub2_HK, aes(LV2012_Fiscal, colour = DiEiecL, linetype = DiEiecL)) + geom_density(size = 1) + 
        scale_color_brewer(palette = 'Set1', name = 'Electoral\nCompetitiveness') +
        scale_y_continuous(breaks = c(0, 0.025, 0.04)) +
        scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('Denisty\n') + xlab('') + ggtitle('Laeven and Valencia (2012) in Honohan & Klingebiel (2003)') +
        theme_bw()

P3 <- ggplot(Main, aes(LV2012_Fiscal, colour = DiEiecL, linetype = DiEiecL)) + geom_density(size = 1) + 
        scale_color_brewer(palette = 'Set1', guide = FALSE) +
        scale_y_continuous(breaks = c(0, 0.025, 0.05)) +
        #scale_color_brewer(palette = 'Set1', name = 'Electoral\nCompetitiveness') +
        scale_linetype_discrete(guide = FALSE) +
        #scale_linetype_discrete(name = 'Electoral\nCompetitiveness') +
        ylab('') + xlab('Fiscal Costs (% GDP)') + ggtitle('All Laeven and Valencia (2012) before 2001') +
        theme_bw()

# Combine
gP1 <- ggplotGrob(P1)
gP2 <- ggplotGrob(P2)
gP3 <- ggplotGrob(P3)

maxWidth = grid::unit.pmax(gP1$widths[2:5], gP2$widths[2:5], gP3$widths[2:5])
gP1$widths[2:5] <- as.list(maxWidth)
gP2$widths[2:5] <- as.list(maxWidth)
gP3$widths[2:5] <- as.list(maxWidth)

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/figures/LV_HK_CompareElect.pdf', width = 10)
  grid.arrange(arrangeGrob(gP1, gP2, gP3, ncol = 1, heights = c(3, 3, 3)))
dev.off()
