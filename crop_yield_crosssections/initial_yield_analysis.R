library(tidyverse)
library(ggthemes)


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
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Acres (millions)") +
  ggtitle("US MPCI Acres by Product Type")
g_acres 

## same as above but premium on the Y axis
g_prem <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = prem/1e9, x = year,  fill = PlanType), 
           data = sob_by_year_plan, stat="identity") +
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
           data = sob_by_year_plan_no_state, stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Loss Ratio (%)") +
  ggtitle("US MPCI Loss Ratio by Product Type")
g_lr

## same as above but only IL
g_lr_il <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_line(aes(y = (indem/prem) * 100 , x = year,  color = PlanType), 
            data = filter(sob_by_year_plan, stAbbr=='IL'), stat="identity") +
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
  scale_x_continuous(limits=c(.4, 1.2)) +
  ggtitle("Yield Product Claim Rate by Coverage Level")
g_cov_level
