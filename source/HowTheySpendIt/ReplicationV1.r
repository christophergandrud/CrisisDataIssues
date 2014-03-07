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
library(DataCombine)
library(psData)
library(countrycode)


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
        ylab('') + xlab('Fiscal Costs (% GDP)') + ggtitle('Laeven and Valencia (2012) before 2001') +
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


#### Eurostat ####

# Eurostat data was gathered from: http://epp.eurostat.ec.europa.eu/portal/page/portal/ government_finance_statistics/excessive_deficit/supplementary_tables_financial_turmoil. Accessed March 2014.
# The data was hand entered into a .csv file located at: https://github.com/christophergandrud/CrisisDataIssues/blob/master/data/Eurostat_CrisisCosts.csv

# Load data and clean
EUCosts <- read.csv('/git_repositories/CrisisDataIssues/data/Eurostat_CrisisCosts.csv', stringsAsFactors = FALSE)

names(EUCosts) <- c("country", "year", "t", "NetCost", "GovAssets", 
                    "GovLiabilities", "ContingentLiabilities")

# Load Eurostat GDP data
GDP <- read.csv('/git_repositories/CrisisDataIssues/data/other/nama_gdp_c_1_Data.csv', stringsAsFactors = FALSE)

GDP <- GDP[, c(2, 1, 5)]
GDP$Value <- gsub(',', '', GDP$Value)
GDP$gdp <- as.numeric(GDP$Value)
GDP$country <- countrycode(GDP$GEO, origin = 'country.name', destination = 'iso2c')
GDP$country[GDP$country == 'GB'] <- 'UK'
GDP <- GDP[, c('country', 'TIME', 'gdp')]
names(GDP) <- c('country', 'year', 'gdp')

# Years left in current term
YearsLeft <- DpiGet(vars = c('yrcurnt', 'stabns'))
YearsLeft$iso2c[YearsLeft$iso2c == 'GB'] <- 'UK'
YearsLeft <- YearsLeft[, -2]
names(YearsLeft) <- c('country', 'year', 'yrcurnt', 'stanbs')
YearsLeft$DiElection[YearsLeft$yrcurnt == 0] <- "Election Year"
YearsLeft$DiElection[YearsLeft$yrcurnt > 0] <- "Not Election Year"

# Merge
Comb <- dMerge(EUCosts, GDP, Var = c('country', 'year'), all.x = TRUE)
Comb <- dMerge(Comb, YearsLeft, Var = c('country', 'year'), all.x = TRUE)


# Create X/GDP
xgdp <- function(data, vars){
  for (i in vars){
    data[, paste0(i, 'PerGdp')] <- (data[, i]/data[, 'gdp']) * 100
  }
  return(data)
}
VarNames <- c("NetCost", "GovAssets", "GovLiabilities", "ContingentLiabilities")
Comb <- xgdp(data = Comb, vars = VarNames)
Comb <- DropNA(Comb, 't')

# Remove Finland (only has one observation year)
Comb <- subset(Comb, country != 'FI')

## Plots ###

# Net costs
NetCosts <- ggplot(Comb, aes(t, NetCostPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
              stat_smooth(method = 'lm', se = FALSE) + 
              scale_x_reverse(breaks = c(4, 0)) + 
              ylab('Net Costs (% GDP)\n') + xlab('\nYears from the Crisis Start') +
              theme_bw()

# Contingent liabilities without Ireland
CombSubNoIE <- subset(Comb, country != 'IE')

CLPlot1 <- ggplot(CombSubNoIE, aes(t, ContingentLiabilitiesPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
            stat_smooth(method = 'lm', se = FALSE) + 
            scale_x_continuous(breaks = c(1, 5)) + 
            geom_hline(yintercept = 0, linetype = 'dotted') +
            ylab('Contingent Liabilities (% GDP)\n') + xlab('\nTime in Years from the Crisis Start') +
            ggtitle('Ordered Chronologically') +
            theme_bw()

CLPlot3 <- ggplot(CombSubNoIE, aes(yrcurnt, ContingentLiabilitiesPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
              stat_smooth(method = 'lm', se = FALSE) + 
              scale_x_reverse(breaks = c(4, 0)) + 
              geom_hline(yintercept = 0, linetype = 'dotted') +
              ylab('Contingent Liabilities (% GDP)\n') + xlab('\nYears to Election') +
              ggtitle('Ordered by Time to Election') +
              theme_bw()



# Contingent liabilities for Ireland
CombSubIE <- subset(Comb, country == 'IE')
CLPlot2 <- ggplot(CombSubIE, aes(t, ContingentLiabilitiesPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
              stat_smooth(method = 'lm', se = FALSE) + 
              scale_x_continuous(breaks = c(1, 5)) + 
              ylab('') + xlab('\n') + ggtitle('') +
              theme_bw()

CLPlot4 <- ggplot(CombSubIE, aes(yrcurnt, ContingentLiabilitiesPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
              stat_smooth(method = 'lm', se = FALSE) + 
              scale_x_reverse(breaks = c(4, 1)) + ggtitle('') +
              ylab('') + xlab('\n') +
              theme_bw()

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/figures/CL_Plot.pdf', width = 10)
  grid.arrange(CLPlot1, CLPlot2, CLPlot3, CLPlot4, nrow = 2, widths = c(7, 1))
dev.off()


# Liabilities
LiaPLot1 <- ggplot(Comb, aes(t, GovLiabilitiesPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
              stat_smooth(method = 'lm', se = FALSE) + 
              scale_x_continuous(breaks = c(1, 5)) + 
              geom_hline(yintercept = 0, linetype = 'dotted') +
              ylab('Realized Liabilities (% GDP)\n') + xlab('\nTime in Years from the Crisis Start') +
              ggtitle('Ordered Chronologically') +
              theme_bw()

LiaPLot2 <- ggplot(Comb, aes(yrcurnt, GovLiabilitiesPerGdp)) + geom_jitter() + facet_grid(. ~ country) +
              stat_smooth(method = 'lm', se = FALSE) + 
              scale_x_reverse(breaks = c(4, 0)) + 
              geom_hline(yintercept = 0, linetype = 'dotted') +
              ylab('Realized Liabilities (% GDP)\n') + xlab('\nYears to Election') +
              ggtitle('Ordered by Time to Election') +
              theme_bw()

pdf('~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/figures/Liabilities.pdf', width = 10)
  grid.arrange(LiaPLot1, LiaPLot2)
dev.off()