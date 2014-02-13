#' Download and combine into one data set Reinhart and Rogoff's (2010) crisis dummy variables
#' 
#' @param urls URLs for each Excel file in the Reinhart and Rogoff data set. See \url{http://www.carmenreinhart.com/data/browse-by-topic/topics/7/}.
#' @param OutCountryID character string. The type of country ID you would like to include in the output file along with the country name. See \code{\link{countrycode}} for available options. 
#' @param standardCountryName logical. Whether or not to standardise the country names variable based on \code{country.name} from  \code{\link{countrycode}}.
#' 
#' @return Returns a data frame with the following columns:
#' \itemize{
#'  \item{\code{iso2c}: }{The ISO two letter country code identifying the country. This can be changed to another country ID system using \code{OutCountryID}}
#'  \item{\code{country}: }{Country names.}
#'  \item{\code{year}: }{The year.}
#'  \item{\code{RR_Independence}: }{Year of independence.}
#'  \item{\code{RR_CurrencyCrisis}: }{Currency crisis.}
#'  \item{\code{RR_InflationCrisis}: }{Inflation crisis.}
#'  \item{\code{RR_StockMarketCrash}: }{Stock market crash.}
#'  \item{\code{RR_SovDebtCrisisDom}: }{Domestic sovereign debt crisis.}
#'  \item{\code{RR_SovDebtCrisisExt}: }{External sovereign debt crisis.}
#'  \item{\code{RR_BankingCrisis}: }{Banking crisis.}
#'  \item{\code{RR_YearlyCrisisTally}: }{Total number of crises per year.}
#'  
#'  @importFrom xlsx loadWorkbook
#'  @importFrom xlsx getSheets
#'  @importFrom xlsx read.xlsx
#'  @importFrom DataCombine MoveFront
#'  @importFrom DataCombine DropNA
#'  
#'  @source
#'  Reinhart, Camen M. and Kenneth S. Rogoff, ''From Financial Crash to Debt Crisis,'' NBER Working Paper 15795, March 2010. Forthcoming in American Economic Review.  
#' 
#' @export

RRCrisisGet <- function(urls = c(
   'http://www.carmenreinhart.com/user_uploads/data/22_data.xls',
   'http://www.carmenreinhart.com/user_uploads/data/35_data.xls', 
   'http://www.carmenreinhart.com/user_uploads/data/23_data.xls',
   'http://www.carmenreinhart.com/user_uploads/data/25_data.xls'), 
   OutCountryID = 'iso2c', 
   standardCountryName = standardCountryName){

  OutData <- data.frame()

  for (i in urls){
    tmp <- tempfile()
    download.file(i, tmp)
    WB <- getSheets(loadWorkbook(tmp))
    
    # Load workbook and find relevant sheet names
    WBNames <- names(WB)
    Droppers <- c('Contents', 'CrisisDefinition', 'CrisisDefinitions', 'Sheet1', 'Sheet3')
    WBNames <- WBNames[!is.element(WBNames, Droppers)]
    
    for (u in WBNames){
      Temp <- read.xlsx(tmp, u)
      # Keep only the year and crisis indicators
      Temp <- Temp[13:nrow(Temp), c(1:9)]
      
      # Extract the country name
      if (u == 'UK'){
        Temp$country <- 'United Kingdom'
      }
      else if (u == 'US'){
        Temp$country <- 'United States'
      }
      else if (u != 'US' & u != 'UK'){
        TempNames <- names(Temp[1])
        CountryName <- gsub('([A-z]+).*', '\\1', TempNames)
        Temp$country <- CountryName
      }
      Temp <- MoveFront(Temp, 'country')
      
      # Rename variables
      names(Temp) <- c('country', 'year', 'RR_Independence', 'RR_CurrencyCrisis', 'RR_InflationCrisis',
                      'RR_StockMarketCrash', 'RR_SovDebtCrisisDom', 'RR_SovDebtCrisisExt',
                      'RR_BankingCrisis', 'RR_YearlyCrisisTally')
      
      message(paste0('Cleaning up Excel sheet for ', u, '.\n'))
      Temp <- DropNA(Temp, c('year', 'RR_BankingCrisis'), message = FALSE)
      OutData <- rbind(OutData, Temp)
    }
  }
  # Clean up country names and add ID
  OutData$country[OutData$country == 'New'] <- 'New Zealand'
  OutData$country[OutData$country == 'South'] <- 'South Africa'
  OutData$country[OutData$country == 'Sri'] <- 'Sri Lanka'
  OutData$country[OutData$country == 'Central'] <- 'Central African Republic'
  OutData$country[OutData$country == 'Costa'] <- 'Costa Rica'
  OutData$country[OutData$country == 'Cote'] <- 'Cote dIvoire'
  OutData$country[OutData$country == 'Costa'] <-
  OutData$country[OutData$country == 'Dominican'] <- 'Domincan Republic'
  OutData$country[OutData$country == 'El'] <- 'El Salvador'
                    
  OutData <- CountryID(data = OutData, OutCountryID = OutCountryID,
                timeVar = 'year', standardCountryName = standardCountryName)
  OutData <- OutData[order(OutData[, OutCountryID], OutData[, 'year']), ]
  return(OutData)
}