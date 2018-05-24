



################## go grab some NASS data 
start_year = 1989
end_year = 2018

# grab the latest NASS crop file from ftp://ftp.nass.usda.gov/quickstats/ and save it
library(RCurl)
url <- "ftp://ftp.nass.usda.gov/quickstats/"
filenames <- getURL(url, ftp.use.epsv = FALSE,dirlistonly = TRUE) 
filenames <- unlist(strsplit(filenames,"[\\\\]|[^[:print:]]",fixed=FALSE)) ## split up the filenames
crop_file <- filenames[grep( 'crops', filenames)]
nass_data_bin <- try(getBinaryURL(paste0(url,crop_file)))
writeBin(nass_data_bin, paste0('./data/', crop_file))
rm(nass_data_bin) # free up memory

nass_crop_data = read_tsv(paste0('./data/', crop_file)) # takes a few minutes... 5.6GB

nass_crop_data %>%
  filter(AGG_LEVEL_DESC == 'STATE' &
           REFERENCE_PERIOD_DESC == 'YEAR' &
           YEAR >= start_year &
           YEAR <= end_year) ->
  annual_crop_data

rm(nass_crop_data) # save some memory

annual_crop_data %>%
  filter(COMMODITY_DESC=='CORN' &
           SHORT_DESC %in% c('CORN, GRAIN - PRODUCTION, MEASURED IN BU','CORN - ACRES PLANTED') &
           SOURCE_DESC == 'SURVEY' ) %>%
  mutate(VALUE = as.double( gsub(",","",VALUE) ) ) -> #kill the commas and make value a double
  corn_annual_nass_data


corn_annual_nass_data %>%
  group_by(STATE_ALPHA, YEAR, SHORT_DESC) %>%
  summarize( VALUE = sum(VALUE)) %>%
  mutate(SHORT_DESC = gsub('CORN, GRAIN - PRODUCTION, MEASURED IN BU', 'corn_production', SHORT_DESC) ) %>%
  mutate(SHORT_DESC = gsub('CORN - ACRES PLANTED', 'corn_acres_planted', SHORT_DESC) ) %>%
  spread(SHORT_DESC, VALUE) %>% 
  mutate(plantedYield =  corn_production / corn_acres_planted ) ->
  corn_yield


corn_yield %>%
  filter(STATE_ALPHA == 'IL') ->
  il_corn_yield

ggplot(il_corn_yield) +
  aes(x=YEAR, y=plantedYield) +
  geom_line()

write_csv(il_corn_yield, './data/il_corn_yield.csv')


############ Commodity price data ##########################################################

# install.packages("Quandl")
library(Quandl)


# have to have a quandl token for this to work. Get one here:
# https://blog.quandl.com/getting-started-with-the-quandl-api
# quandlToken <- 'yourToken'
Quandl.api_key(quandlToken)

## Corn
# only grabbing corn data for illustration. So going to get 2-1 to 10-31 data for 1989-2017

#function to grab a single year of December (CZ) corn data and shove it into a text file
download_quandl_data <- function(year) {
  contractString <- paste0('CME/CZ', year)
  # grab the data from quandl
  year_data = Quandl(contractString)
  Sys.sleep(3) # slow down a bit
  write_csv(year_data, paste0("./data/CMECZ", year, '.csv'))
}

years <- 1989:2017
lapply(years, download_quandl_data)



### calculate the price change for a given year
calc_corn_price_change <- function(year) {
  year_data = read_csv(paste0("./data/CMECZ", year, '.csv'))
  
  # filter to date range desired
  # calculate a column holding only the month
  year_data %>%
    filter(Date >= paste0(year, '-02-01') & Date <= paste0(year, '-10-31')) %>% 
    mutate(month=format(Date, "%m")) %>%
    mutate( period = case_when( month=='10' ~ 'H', 
                                month=='02' ~ 'S')) ->
    year_data
  
  year_data %>% 
    filter(!is.na(period)) %>%
    group_by(period) %>%
    summarize(avg_price = mean(Settle)) %>%
    spread(period, avg_price) %>%
    summarize(change = (H - S) / H ) ->
    price_change 
  
  return(price_change[[1]])
}

corn_prices <- data.frame(year = years, 
                          price_change = unlist(lapply(years, calc_corn_price_change))
                          )

write_csv(corn_prices, './data/corn_prices.csv')

#######################################  RMA Summary of Business   #####################

## summary of business files are downloaded from here:
## http://www.rma.usda.gov/data/sob/scc/index.html

# we can pull them in mass, however
# set up a function to either download the data, or load it from local files
# depending on how it's called. 
load_rma_data <- function(remote=TRUE, years=1989:2018){
  sob_list <- vector(mode = "list", length = length(years))
  i <- 1 ## using i to track which element of sob_list we are popping the 
  ## data into 
  
  for (year in years) {
    if (remote) { # download the remote file if remote == true, otherwise assume
      # the data is in the ./data/ directory and properly named
      url <- paste0('https://www.rma.usda.gov/data/sob/sccc/sobcov_', year, '.zip')
      download.file(url, dest)
    }
    
    dest <- paste0('./data/sobcov_', year, '.zip')
    df_sob <- read_delim(dest, delim='|',
                         col_names = c('year','stFips','stAbbr','coFips','coName',
                                       'cropCd','cropName','planCd','planAbbr','coverCat',
                                       'deliveryType','covLevel','policyCount','policyPremCount','policyIndemCount',
                                       'unitsReportingPrem', 'indemCount','quantType', 'quantNet', 'companionAcres',
                                       'liab','prem','subsidy','indem', 'lossRatio'), 
                         trim_ws = TRUE)
    
    ## aggregate to state level, kicking out the county data
    ## since we want only high level view 
    df_sob %>%
      mutate(planCd = trimws(as.character(planCd))) %>% # early years are int, later are char
      group_by(year,stFips,stAbbr, cropCd,cropName,planCd,planAbbr,
               covLevel, quantType ) %>%
      summarize(liab=sum(as.numeric(liab)), 
                prem = sum(as.numeric(prem)), 
                indem = sum(as.numeric(indem)), 
                indemCount = sum(as.numeric(indemCount)), 
                unitsReportingPrem = sum(as.numeric(unitsReportingPrem)), 
                quantNet = sum(as.numeric(quantNet))
      ) ->
      df_sob_state
    sob_list[[i]] <- df_sob_state
    i <- i+1
  }
  
  sob <- bind_rows(sob_list)
  return(sob)
}

# load the data. If this has been run before, the files should be local, 
# so set remote = FALSE
sob <- load_rma_data(remote=TRUE, years=1989:2018)


## do some quick profiling of the sob data
sob %>%
  filter( quantType == 'Acres') %>%
  mutate(PlanType = case_when( planCd %in% c('25','42','44','45','73', '02', '03', '05', '06') ~ 'Revenue', 
                               TRUE ~ 'Yield')) %>%
  group_by(year, PlanType, stAbbr) %>%
  summarize(liab = sum(liab), prem=sum(prem), indem=sum(indem), 
            acres=sum(quantNet, na.rm=TRUE)) ->
  sob_by_state_year_plan

write_csv(sob_by_state_year_plan, './data/sob_by_state_year_plan.csv')

