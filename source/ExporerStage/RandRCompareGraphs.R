
library(plyr)
library(reshape2)
library(ggplot2)


# Load data 
CombRR <- read.csv(file = '~/Dropbox/AMCProject/MissingLinkDataAnalysis/data/ReinhartRogoffLVCount.csv')


#### Compare yearly total of crises
LV <- ddply(CombRR, .(year), summarize, LVSum = sum(LV_SystemCrisis, na.rm = TRUE))
RR <- ddply(CombRR, .(year), summarize, RRSum = sum(RR_BankingCrisis, na.rm = TRUE))

Comb <- merge(RR, LV, by = c('year'))

CombMolten <- melt(Comb, id.vars = 'year')

ggplot(CombMolten, aes(year, value, colour = variable)) + 
  geom_line() + 
  xlab('') + ylab('No. of Crises Per Year\n') +
  scale_color_brewer(palette = 'Set1', name = '') +
  scale_x_continuous(limits = c(1975, 2010)) +
  theme_bw()

# Plot polity during crisis
## CombRRSub <- subset(CombRR, RR_BankingCrisis == 1)
CombRR$polity2[CombRR$RR_BankingCrisis == 0] <- NA

# Jitter polity
CombRR <- ddply(CombRR, .(country), transform, rand = runif(1, -1, 1))
CombRR$PolityJitter <- CombRR$polity2 + CombRR$rand

ggplot(CombRR, aes(year, polity2)) + geom_jitter() + 
  geom_smooth(se = FALSE) + 
  xlab('') + ylab('Polity IV\n') +
  #geom_vline(aes(xintercept = c(1975, 2000))) + 
  theme_bw()

ggplot(CombRR, aes(year, PolityJitter)) + geom_point(aes(colour = country)) + geom_line(aes(colour = country), size = 1.5) + 
  #geom_smooth(se = FALSE) + 
  xlab('') + ylab('Polity IV\n') +
  #geom_vline(aes(xintercept = c(1975, 2000))) + 
  scale_x_continuous(limits = c(1973, 2010)) +
  scale_color_discrete(guide = FALSE) +
  theme_bw()

ggplot(CombRR, aes(year, PolityJitter)) + geom_line(aes(group = country, colour = RR_BankingCrisis, alpha = 0.1), size = 1.5) + 
  #geom_smooth(se = FALSE) + 
  xlab('') + ylab('Polity IV\n') +
  #geom_vline(aes(xintercept = c(1975, 2000))) + 
  scale_color_manual(values = c('grey', 'red'), guide = FALSE) +
  scale_alpha(guide = FALSE) +
  theme_bw()

ggplot(CombRR, aes(year, PolityJitter)) + geom_line(aes(group = country, colour = RR_BankingCrisis, alpha = 0.1), size = 1.5) + 
  #geom_smooth(se = FALSE) + 
  xlab('') + ylab('Polity IV\n') +
  #geom_vline(aes(xintercept = c(1975, 2000))) + 
  scale_color_manual(values = c('grey', 'red'), guide = FALSE) +
  scale_alpha(guide = FALSE) +
  scale_x_continuous(limits = c(1973, 2010)) +
  theme_bw()