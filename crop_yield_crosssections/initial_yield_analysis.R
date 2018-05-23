library(tidyverse)
library(ggthemes)
library(scales)


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
sob_by_year_plan

## US MPCI Acres by Product Type (Revenue vs. Yield)
g_acres <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = acres/1e6, x = year,  fill = PlanType), 
           data = sob_by_year_plan, stat="identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Acres (millions)") +
  ggtitle("US MPCI Acres by Product Type")
g_acres 

## same as above but premium on the Y axis
g_prem <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = prem/1e9, x = year,  fill = PlanType), 
           data = sob_by_year_plan, stat="identity")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Premium ($ B)") +
  ggtitle("US MPCI Premium by Product Type")
g_prem

## kick out the state grouping then plot loss ratio over time by product type
sob_by_year_plan %>%
  group_by(year, PlanType) %>% #changed this grouping is all we do here
  summarize(liab = sum(liab), prem=sum(prem), indem=sum(indem), 
            acres=sum(acres, na.rm=TRUE)) ->
sob_by_year_plan_no_state

# now plot loss ratio over time by group
g_lr <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_line(aes(y = indem / prem , x = year,  color = PlanType), 
           data = sob_by_year_plan_no_state, stat="identity")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Loss Ratio (%)") +
  ggtitle("US MPCI Loss Ratio by Product Type")
g_lr

## same as above but only IL
g_lr_il <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_line(aes(y = (indem/prem) * 100 , x = year,  color = PlanType), 
            data = filter(sob_by_year_plan, stAbbr=='IL'), stat="identity")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Loss Ratio (%)") +
  ggtitle("Illinois MPCI Loss Ratio by Product Type")
g_lr_il

## let's pull out IL CORN and split it out by coverage level
## see if we can see a pattern in the shape of the underlying
sob %>% 
  filter(quantType == 'Acres',        # field crops only 
         planAbbr %in% c('APH','YP'), # yield only
         stAbbr == 'IL', cropName == 'CORN', 
         covLevel > .5) %>%          # kick out the low cover levels
  group_by(year,covLevel) %>% 
  summarize(claim_rate = sum(indemCount) / sum(unitsReportingPrem), 
            prem = sum(prem), 
            indem = sum(indem), 
            lr = sum(indem) / sum(prem)) %>%
  filter(year %in% c(2015,2016,2012)) -> # 2016-g 2015-m 2012-b
claims_rate_covlevel

sob %>% 
  filter(quantType == 'Acres',        # field crops only 
         planAbbr %in% c('APH','YP'), # yield only
         stAbbr == 'IL', cropName == 'CORN', 
         covLevel > .5) %>%          # kick out the low cover levels
  group_by(year,covLevel) %>% 
  summarize(claim_rate = sum(indemCount) / sum(unitsReportingPrem), 
            prem = sum(prem), 
            indem = sum(indem), 
            lr = sum(indem) / sum(prem)) %>%
  filter(year %in% c(2002,2005)) ->
claims_rate_covlevel


## for the 3 years we selected above let's plot claim rate vs cover level
## grouped by year so we can better understand the shape
g_cov_level <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_point(aes(y = claim_rate*100 , x = covLevel,  color = as.character(year)), 
             data = claims_rate_covlevel, stat="identity") +
  geom_line(aes(y = claim_rate*100 , x = covLevel,  color = as.character(year)), 
            data = claims_rate_covlevel, stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Coverage Level", y="Claim Rate (%)") +
  scale_x_continuous(limits=c(.5, 1), breaks = scales::pretty_breaks(n = 10))  +
  ggtitle("Yield Protection: Claim Rate by Coverage Level")
g_cov_level


## create 3 CDFs to illustrate what's going on with the prior graph
c_level <- seq( from=0, to=1.5, by=.1)

plot(function(x) pbeta(x, shape1=20, shape2=5 ), .5, 1)  
plot(function(x) pbeta(x, shape1=2, shape2=6 ), .5, 1, add=TRUE)
#plot(function(x) pbeta(x, shape1=2, shape2=8 ), 0, 1, add=TRUE)

plot(function(x) pbeta(x, shape1=9, shape2=8 ), .5, 1, add=TRUE, col='blue')
plot(function(x) pbeta(x, shape1=9, shape2=10 ), .5, 1, add=TRUE, col='blue')


fit_beta_cdf <- function(x,y) {
  beta_func <- function(par, x) sum( (pbeta( x, par[1], par[2]) - y)**2 ) 
  #out <- optim(c(1,.8), beta_func, method="CG", x=x)
  out <- optim(c(1,.8), lower=c(1,.9), upper=c(100,100), beta_func, method="L-BFGS-B", x=x)
  
  #plot(function(x) pbeta(x, shape1=out$par[1], shape2=out$par[2] ), 0, 1.5, col='red')
  #lines(x,y, col='blue')
  plot(function(x) dbeta(x, shape1=out$par[1], shape2=out$par[2] ), 0, 1.5)
  return(out$par) 
}

beta_on_group <- function(dat){
  params <- fit_beta_cdf(dat$covLevel, dat$claim_rate) 
  out <- data.frame(year = unique(dat$year), 
                    b1 = params[1], 
                    b2 = params[2])
  return(out)
}
claims_rate_covlevel %>%
  group_by(year) %>%
  do(beta_on_group(.)) ->
m

f1 <- function(x) pbeta(x, shape1=1.01, shape2=.9 ) * 100


g_cov_level +
layer(stat = "function",
      fun = f1, 
      mapping = aes(color = "f1") # Give a meaningful name to color
) 

g_cov_level +
  stat_function(fun = f1)

f <- ggplot(data.frame(x = c(0, 1)), aes(x))

f + stat_function(fun = f1)



############ Commodity price data 

# install.packages("Quandl")
library(Quandl)


# have to have a quandl token for this to work. Get one here:
# https://blog.quandl.com/getting-started-with-the-quandl-api
# quandlToken <- 'yourToken'
Quandl.api_key(quandlToken)

## Corn
# only grabbing corn data for illustration. So going to get 2-1 to 10-31 data for 1989-2017

plot_corn_prices <- function(year, download=TRUE) {
  contractString <- paste0('CME/CZ', year)
  
  if (download) {
    # grab the data from quandl
    year_data = Quandl(contractString)
    Sys.sleep(3) # slow down a bit
    write_csv(year_data, paste0("./data/CMECZ", year, '.csv'))
  } else {
    year_data = read_csv(paste0("./data/CMECZ", year, '.csv'))
    
  }
  
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
  
  price_change <- paste0(round(price_change[[1]], digits=2) * 100, '%')
  
  ## shade spring and harvest price - repeats logic from above but in 
  ## a different format. Should not have to repeat, but can't make it work
  ## otherwise
  rects <- data.frame(startDate = as.Date(c(paste0(year, '-02-01'),paste0(year, '-10-01'))), 
                      endDate = as.Date(c(paste0(year, '-02-28'),paste0(year, '-10-31'))), 
                      col = c('S','H'))
  
  g_price <- ggplot() + theme_economist() + scale_fill_economist() +
    geom_rect(data = rects,  #bring in shading rectangles
              aes(xmin = startDate, xmax = endDate, 
                         ymin = -Inf, ymax = Inf, 
                         fill = col), 
              alpha = 0.4) +
    geom_line(aes(y = Settle/100  , x = Date),  # plot settle price
              data = year_data, stat="identity") +
    expand_limits(y=0) + # make x axis go to 0, like it should
    scale_x_date(labels = date_format("%b-%y"), 
                 breaks = scales::pretty_breaks(n = 9)) + # one break per month
    theme(axis.text.x = element_text(angle = -90, hjust = 1)) + #rotate dates
    theme(legend.position="none") + # kill legend
    labs(x="Date", y="CME Price" ) +
    ggtitle(paste0("CME Corn (CZ) Daily Settle Price - ", year), 
            subtitle = paste0('Price Δ Feb vs Oct = ', price_change)) 
  g_price
  return(g_price)
}


plot_corn_prices(2013, download=TRUE)

years <- 1989:2017
graph_list <- lapply(years, plot_corn_prices)

list_to_plot <- graph_list[c(20, 24, 27, 28)]
library(gridExtra)
do.call("grid.arrange", c(list_to_plot, ncol=2))



################## go grab some NASS data 

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

start_year = 1989
end_year = 2018

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
         