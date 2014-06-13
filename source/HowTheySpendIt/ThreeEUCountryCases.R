###############
# Compare ex post recorded contingent and realised costs
# Christopher Gandrud
# 13 June 2014
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

PTPlot <- ggplot(PT, aes(year, value, group = variable, color = variable)) +
            geom_point(size = 3) +
            geom_line() +
            geom_vline(xintercept = c(2009, 2011), linetype = 'dashed') +
            scale_color_manual(values = c('#2c7fb8', '#fdae6b'), 
                               guide = FALSE) +
            xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Portugal\n') +
            theme_bw()

#### Germany ####

DE = subset(MoltenCosts, country == 'DE')

DEPlot <- ggplot(DE, aes(year, value, group = variable, color = variable)) +
            geom_point(size = 3) +
            geom_line() +
            geom_vline(xintercept = c(2009, 2013), linetype = 'dashed') +
            scale_color_manual(values = c('#2c7fb8', '#fdae6b'), 
                               guide = FALSE) +
            xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Germany\n') +
            theme_bw()

#### Ireland ####
IE = subset(MoltenCosts, country == 'IE')

IEPlot <- ggplot(IE, aes(year, value, group = variable, color = variable)) +
    geom_point(size = 3) +
    geom_line() +
    geom_vline(xintercept = c(2011), linetype = 'dashed') +
    scale_color_manual(values = c('#2c7fb8', '#fdae6b'), 
                       name = 'Liabilities') +
    xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Ireland\n') +
    theme_bw()


#### Combine Plots ####
# Combine
gP1 <- ggplotGrob(PTPlot)
gP2 <- ggplotGrob(DEPlot)
gP3 <- ggplotGrob(IEPlot)

maxWidth = grid::unit.pmax(gP1$widths[2:5], gP2$widths[2:5], 
                           gP3$widths[2:5])
gP1$widths[2:5] <- as.list(maxWidth)
gP2$widths[2:5] <- as.list(maxWidth)
gP3$widths[2:5] <- as.list(maxWidth)


pdf('~/Desktop/EuroReWrite/EU_3CasesCompare.pdf', width = 10) ## Change later
    grid.arrange(arrangeGrob(gP1, gP2, gP3, ncol = 1, 
                             heights = c(3, 3, 3)))
dev.off()


#### Ireland Revisions ####

# Create data frame from sources: 
# http://epp.eurostat.ec.europa.eu/portal/page/portal/government_finance_statistics/documents/IE_2011-10_2nd.pdf
# http://epp.eurostat.ec.europa.eu/portal/page/portal/government_finance_statistics/documents/IE_2012-04.pdf
# http://epp.eurostat.ec.europa.eu/portal/page/portal/government_finance_statistics/excessive_deficit/supplementary_tables_financial_turmoil

type <- c('Original EDPT', 'Revised EDPT', 'Original + Financial\nCrisis Liabilities', 
          'Original EDPT', 'Revised EDPT', 'Original + Financial\nCrisis Liabilities')
year <- c(2009, 2009, 2009, 
          2010, 2010, 2010)
value <- c(5942, 11411, 12059, 
           5943, 42987, 44146)
    
Comb <- data.frame(type, year, value)

# Plot

pdf('~/Desktop/EuroReWrite/IrelandNumbersCompare.pdf', width = 10) ## Change later
ggplot(Comb, aes(as.factor(year), value, group = type, linetype = type,
                 color = type)) +
    geom_point(size = 3) +
    geom_line(size = 1) +
    scale_color_brewer(palette = 'Set1', name = 'Data source') +
    scale_linetype_discrete(name = 'Data source') +
    xlab('') + ylab('Millions of Euros\n') +
    theme_bw(base_size = 15)
dev.off()



#### Slovenia Test ####
SI = subset(MoltenCosts, country == 'SI')

# Create individual tables numbers (rather than all country sup. table)
Temp = SI[1:7, ]
Temp$variable <- 'Indv. Tables\nImmediately\nRealized'

Temp[7, 4] <- 13.82

SINew <- rbind(Temp, SI)

ggplot(SINew, aes(year, value, group = variable, color = variable)) +
    geom_point(size = 3) +
    geom_line() +
    geom_vline(xintercept = c(2008, 2011), linetype = 'dashed') +
    scale_color_manual(values = c('#2c7fb8', '#fdae6b', '#e41a1c'), 
                       name = '') +
    xlab('') + ylab('% of 2008 GDP\n') + ggtitle('Slovenia\n') +
    theme_bw()

