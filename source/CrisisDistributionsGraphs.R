################
# Create crisis/political institutions distributions graphs
# Christopher Gandrud
# 20 February 2014
################

library(foreign)
library(ggplot2)
library(gridExtra)
library(reshape2)

Main <- read.dta('/git_repositories/CrisisDataIssues/data/KeeferExtended.dta')
LV <- subset(Main, LV2012_Fiscal >= 0)
Keefer <- subset(Main, Keefer2007_Fiscal >= 0)

#### Distribution among crisis countries ####
Plot1 <- ggplot(LV, aes(year, polity2)) + geom_jitter() + geom_smooth(se = FALSE) +
              xlab('') + ylab('Polity IV\n') +
              theme_bw()

Plot2 <- ggplot(LV, aes(year, checks)) + geom_jitter() + geom_smooth(se = FALSE) +
              xlab('') + ylab('Checks\n') +
              theme_bw()

Plot3 <- ggplot(LV, aes(year, eiec)) + geom_jitter() + geom_smooth(se = FALSE) + 
                xlab('') + ylab('Electoral Competitiveness\n') +
                theme_bw()

Plot4 <- ggplot(LV, aes(year, stabnsLag3)) + geom_jitter() + geom_smooth(se = FALSE) + 
                xlab('') + ylab('Political Instability\n(3 year lagged average)\n') +
                theme_bw()

Plot5 <- ggplot(LV, aes(year, W)) + geom_jitter() + geom_smooth(se = FALSE) + 
          xlab('') + ylab('Winset\n') +
          theme_bw()

Plot6 <- ggplot(LV, aes(year, log(GDPperCapita))) + geom_jitter() + geom_smooth(se = FALSE) + 
            scale_y_continuous(limits = c(0, 12), breaks = c(0, 5, 10)) +
            xlab('') + ylab('Log GDP per Capita\n') +
            theme_bw()

# Combine Plots
pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/figures/PolCris.pdf')
grid.arrange(Plot1, Plot2, Plot3, Plot4, Plot5, Plot6, ncol = 2)
dev.off()

#### Plot of revisions ####
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

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/figures/FiscalDifference.pdf', width = 10)
PlotDiff
dev.off()

#### Directly plot LV vs. HL
#Sub <- Main[, c('year', 'Honohan2003_Fiscal', 'LV2012_Fiscal')]
#SubMolten <- melt(Sub, id.vars = 'year')
#ggplot(SubMolten, aes(year, value)) + geom_jitter() + facet_grid(.~variable) + theme_bw()


#### Density/Instability ####
Sub <- Main[, c('stabnsLag3', 'LV2012_Fiscal', 'Honohan2003_Fiscal', 'Keefer2007_Fiscal')]
SubMolten <- melt(Sub, id.vars = c('stabnsLag3'))
SubMolten$variable <- as.character(SubMolten$variable)
SubMolten$variable[SubMolten$variable == 'Honohan2003_Fiscal'] <- 'Honohan/Kling.'
SubMolten$variable[SubMolten$variable == 'Keefer2007_Fiscal'] <- 'Keefer'
SubMolten$variable[SubMolten$variable == 'LV2012_Fiscal'] <- 'Laeven/Valencia'

# Density compare
PlotDensity <- ggplot(SubMolten, aes(value, colour = as.factor(variable))) + 
  geom_density(aes(linetype = as.factor(variable))) + 
  xlab('\nFiscal Costs of Crisis (% GDP)') + ylab('Density\n') +
  scale_color_manual(values = c('#e41a1c', '#377eb8', '#4daf4a'), name = 'Data Set') +
  scale_linetype(name = 'Data Set') +
  theme_bw()

PlotInstCosts <- ggplot(SubMolten, aes(value, stabnsLag3, colour = as.factor(variable))) +
  scale_color_manual(values = c('#e41a1c', '#377eb8', '#4daf4a'), name = '') +
  geom_point(size = 5, alpha = 0.5) +
  xlab('\nFiscal Costs of Crisis (% GDP)') + ylab('Political Instability\n(3 year lagged average)\n') +
  theme_bw()

pdf(file = '~/Dropbox/AMCProject/CrisisDataIssuesPaper/figures/InstCosts.pdf')
grid.arrange(PlotDensity, PlotInstCosts)
dev.off()