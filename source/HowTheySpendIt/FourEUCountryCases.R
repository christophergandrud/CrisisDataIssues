###############
# Compare ex post recorded contingent and realised costs
# Christopher Gandrud
# 16 May 2014
###############

#### Load packages ####
library(repmis)
library(reshape2)
library(ggplot2)
library(gridExtra)

#### Gather and clean data (originally from Eurostat) ####
Costs = source_data('https://raw.githubusercontent.com/christophergandrud/CrisisDataIssues/master/data/Eurostat_CrisisCosts.csv')

CostSub = Costs[, c('country', 'year', 'GovLiabilitiesPerGdp_2008', 
                    'ContingentLiabilitiesPerGdp_2008')]

MoltenCosts = melt(CostSub, id.vars = c('country', 'year'))

MoltenCosts$variable <- factor(MoltenCosts$variable, 
                            labels = c('Immediately Realized', 'Contingent'))

#### Portugal ###
PT = subset(MoltenCosts, country == 'PT')

ggplot(PT, aes(year, value, group = variable)) +
    geom_line(aes(color = variable)) +
    geom_vline(xintercept = c(2009, 2011), linetype = 'dashed') +
    annotate("text", x = 2008, y = 8.5, label = "Parliamentary\n Election", 
             colour = "grey50") +
    scale_color_manual(values = c('#2c7fb8', '#fdae6b'), name = 'Liabilities') +
    xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Portugal\n') +
    theme_linedraw()

#### Ireland ####

IE = subset(MoltenCosts, country == 'IE')

ggplot(IE, aes(year, value, group = variable)) +
    geom_line(aes(color = variable)) +
    geom_vline(xintercept = c(2011), linetype = 'dashed') +
    scale_color_manual(values = c('#2c7fb8', '#fdae6b'), name = 'Liabilities') +
    xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Ireland\n') +
    theme_linedraw()

#### Germany ####

DE = subset(MoltenCosts, country == 'DE')

ggplot(DE, aes(year, value, group = variable)) +
    geom_line(aes(color = variable)) +
    geom_vline(xintercept = c(2009, 2013), linetype = 'dashed') +
    scale_color_manual(values = c('#2c7fb8', '#fdae6b'), name = 'Liabilities') +
    xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Germany\n') +
    theme_linedraw()


#### Spain ####

ES = subset(MoltenCosts, country == 'ES')

ggplot(ES, aes(year, value, group = variable)) +
    geom_line(aes(color = variable)) +
    geom_vline(xintercept = c(2008, 2011), linetype = 'dashed') +
    scale_color_manual(values = c('#2c7fb8', '#fdae6b'), name = 'Liabilities') +
    xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Spain\n') +
    theme_linedraw()
