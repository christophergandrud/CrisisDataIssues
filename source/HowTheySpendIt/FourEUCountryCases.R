###############
# Compare ex post recorded contingent and realised costs
# Christopher Gandrud
# 28 May 2014
###############

#### Load packages ####
library(repmis)
library(reshape2)
library(ggplot2)
library(DataCombine)
library(gridExtra)

#### Gather and clean data (originally from Eurostat) ####
CostsURL <- 'https://raw.githubusercontent.com/christophergandrud/CrisisDataIssues/master/data/Eurostat_CrisisCosts.csv'
Costs = source_data(CostsURL, cache = TRUE)

CostSub = Costs[, c('country', 'year', 'GovLiabilitiesPerGdp_2008', 
                    'ContingentLiabilitiesPerGdp_2008')]

MoltenCosts = melt(CostSub, id.vars = c('country', 'year'))

MoltenCosts$variable <- factor(MoltenCosts$variable, 
                            labels = c('Immediately\nRealized', 'Contingent'))

#### Portugal ###
PT = subset(MoltenCosts, country == 'PT')

PTPlot <- ggplot(PT, aes(year, value, group = variable)) +
            geom_line(aes(color = variable)) +
            geom_vline(xintercept = c(2009, 2011), linetype = 'dashed') +
            scale_color_manual(values = c('#2c7fb8', '#fdae6b'), 
                               guide = FALSE) +
            xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Portugal\n') +
            theme_bw()

#### Ireland ####
IE = subset(MoltenCosts, country == 'IE')

IEPlot <- ggplot(IE, aes(year, value, group = variable)) +
            geom_line(aes(color = variable)) +
            geom_vline(xintercept = c(2011), linetype = 'dashed') +
            scale_color_manual(values = c('#2c7fb8', '#fdae6b'), 
                               name = 'Liabilities') +
            xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Ireland\n') +
            theme_bw()

#### Germany ####

DE = subset(MoltenCosts, country == 'DE')

DEPlot <- ggplot(DE, aes(year, value, group = variable)) +
            geom_line(aes(color = variable)) +
            geom_vline(xintercept = c(2009, 2013), linetype = 'dashed') +
            scale_color_manual(values = c('#2c7fb8', '#fdae6b'), 
                               guide = FALSE) +
            xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Germany\n') +
            theme_bw()


#### Spain ####

ES = subset(MoltenCosts, country == 'ES')

ESPlot <- ggplot(ES, aes(year, value, group = variable)) +
            geom_line(aes(color = variable)) +
            geom_vline(xintercept = c(2008, 2011), linetype = 'dashed') +
            scale_color_manual(values = c('#2c7fb8', '#fdae6b'), guide = FALSE) +
            xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Spain\n') +
            theme_bw()


#### Combine Plots ####
# Combine
gP1 <- ggplotGrob(PTPlot)
gP2 <- ggplotGrob(IEPlot)
gP3 <- ggplotGrob(DEPlot)
gP4 <- ggplotGrob(ESPlot)

maxWidth = grid::unit.pmax(gP1$widths[2:5], gP2$widths[2:5], 
                           gP3$widths[2:5], gP4$widths[2:5])
gP1$widths[2:5] <- as.list(maxWidth)
gP2$widths[2:5] <- as.list(maxWidth)
gP3$widths[2:5] <- as.list(maxWidth)
gP4$widths[2:5] <- as.list(maxWidth)


pdf('~/Desktop/EuroReWrite/EU_4CasesCompare.pdf', width = 10) ## Change later
    grid.arrange(arrangeGrob(gP1, gP2, gP3, gP4, ncol = 2, 
                             heights = c(3, 3, 3, 3)))
dev.off()
