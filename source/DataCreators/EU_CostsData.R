##############
# Download and clean EU Financial Crisis costs data
# Christopher Gandrud
# 24 April 2014
##############

# Load packages
library(repmis)
library(reshape2)

#### Download data ####
sr <- 4 # start row
er <- 32 # end row

EUurl <- 'http://epp.eurostat.ec.europa.eu/portal/page/portal/government_finance_statistics/documents/Summary_table_for_the_financial_crisis-Apr.2014.xlsx'

Main <- source_XlsxData(EUurl, sheet = 1, startRow = sr, endRow = er)

#### Load locally (if needed) ####
library(xlsx)
DataDir <- '~/Desktop/'
Main <- read.xlsx(file = paste0(DataDir, 
                    'Summary_table_for_the_financial_crisis-Apr.2014.xlsx'),
                  sheetIndex = 1, startRow = sr, endRow = er, 
                  colClasses = 'character')

#### Cleaning ####
# Convert 0 to NA
for (i in 2:29){
    Main[, i][Main[, i] == 0] <- NA
    Main[, i] <- round(Main[, i], digits = 1)
}

#### Extract seperate series ####
SeriesExtractor <- function(data = Main, colIndex, newName){
    temp <- data[, colIndex]
    temp <- melt(temp, id.vars = 1)
    names(temp) <- c('country', 'year', newName)
    temp$year <- gsub('X', '', temp$year)
    temp$year <- gsub('\\.[1-9]', '', temp$year)
    
    temp$year <- as.numeric(temp$year)
    temp <- temp[order(temp$country, temp$year), ]
    return(temp)
}

NC <- SeriesExtractor(colIndex = c(1, 2:8), newName = 'NetCost')
GA <- SeriesExtractor(colIndex = c(1, 9:15), newName = 'GovAssets')
GL <- SeriesExtractor(colIndex = c(1, 16:22), newName= 'GovLiabilities')
CL <- SeriesExtractor(colIndex = c(1, 23:29), newName = 'ContingentLiabilities')

# Merge back together
b <- c('country', 'year')
Comb <- merge(NC, GA, by = b)
Comb <- merge(Comb, GL, by = b)
Comb <- merge(Comb, CL, by = b)

# Replace NA with 0 for years where at least one quantity is reported
for (i in 1:nrow(Comb)){
    test <- any(!is.na(Comb[i, 3:6]))
    if (isTRUE(test)){
        for (u in 3:6){
            Comb[i, u][is.na(Comb[i, u])] <- 0
        }
    }
}

# Creat chronological crisis counts
Comb$time <- Comb$year - 2006
Comb <- MoveFront(Comb, c('country', 'year', 'time'))

# Save
SaveDir <- '/git_repositories/CrisisDataIssues/data/'
write.csv(Comb, file = paste0(SaveDir, 'Eurostat_CrisisCosts.csv'),
          row.names = FALSE)
