#####################
# Country sample table creator
# Christopher Gandrud
# 14 March 2014
#####################

# Load data
KeeferFiscal <- read.csv('/git_repositories/CrisisDataIssues/data/KefferFiscal.csv', 
                         stringsAsFactors = FALSE)

# Correlate
cor.test(KeeferFiscal$LV2012.Fiscal, KeeferFiscal$Keefer2007.Fiscal)

# Clean
KeeferFiscal$Temp <- is.na(KeeferFiscal$LV2012.Fiscal) & is.na(KeeferFiscal$Keefer2007.Fiscal) & is.na(KeeferFiscal$Honohan2003.Fiscal)
KeeferFiscal <- subset(KeeferFiscal, Temp == FALSE)

KeeferFiscalTable <- KeeferFiscal[, 2:6]
names(KeeferFiscalTable) <- c('Country', 'Crisis', 'Laeven/Valencia', 'Keefer', 'Honohan/Klingebiel')

KeeferFiscalTable$Country[KeeferFiscalTable$Country == 'MACEDONIA, THE FORMER YUGOSLAV REPUBLIC OF'] <- 'MACEDONIA'
KeeferFiscalTable$Country[KeeferFiscalTable$Country == 'BOLIVIA, PLURINATIONAL STATE OF'] <- 'BOLIVIA'
KeeferFiscalTable$Country[KeeferFiscalTable$Country == 'TANZANIA, UNITED REPUBLIC OF'] <- 'TANZANIA'
KeeferFiscalTable$Country[KeeferFiscalTable$Country == 'VENEZUELA, BOLIVARIAN REPUBLIC OF'] <- 'VENEZUELA'
KeeferFiscalTable$Country[KeeferFiscalTable$Country == 'Japan'] <- 'JAPAN'

print(xtable(KeeferFiscalTable, 
             caption = 'Comparison of Fiscal Costs as a Percentage of GDP Variables from Laeven and Valencia (2012), Keefer (2007), and Honohan and Klingebiel (2003)', 
             label = 'DVTable',
             digits = c(0, 0, 0, 1, 2, 1)), 
      size = 'tiny', include.rownames = FALSE, caption.placement = 'top', 
      file = '~/Dropbox/AMCProject/CrisisDataIssuesPaper/HowYouSpendWriteUp/tables/CountryLists.tex')