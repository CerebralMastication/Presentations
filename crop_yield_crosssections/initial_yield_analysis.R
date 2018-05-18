library(tidyverse)
library(ggthemes)
library(glue)

## summary of business files are downloaded from here:
## http://www.rma.usda.gov/data/sob/scc/index.html

# we can pull them in mass, however
years <- 1989:2017
sob_list <- vector(mode = "list", length = length(years))
i <- 1

for (year in years) {
  # url <- paste0('https://www.rma.usda.gov/data/sob/sccc/sobcov_', year, '.zip')
  dest <- paste0('./data/sobcov_', year, '.zip')
  # download.file(url, dest)
  
  df_sob <- read_delim(dest, delim='|',
                   col_names = c('year','stFips','stAbbr','coFips','coName',
                                 'cropCd','cropName','planCd','planAbbr','coverCat',
                                 'deliveryType','covLevel','policyCount','policyPremCount','policyIndemCount',
                                 'unitsReportingPrem', 'indemCount','quantType', 'quantNet', 'companionAcres',
                                 'liab','prem','subsidy','indem', 'lossRatio'))
  ## aggregate to state level
  df_sob %>%
    mutate(planCd = trimws(as.character(planCd))) %>% # early years are int, later are char
    group_by(year,stFips,stAbbr, cropCd,cropName,planCd,planAbbr,
             covLevel, quantType = trimws(quantType) ) %>%
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

## fix issues with trailing spaces on quantType
# sob %>% 
#   mutate( quantType = trimws(quantType)) ->
# sob


## do some quick profiling of the sob data
sob %>%
  filter( quantType == 'Acres') %>%
  mutate(PlanType = case_when( planCd %in% c('25','42','44','45','73', '02', '03', '05', '06') ~ 'Rev', 
                               TRUE ~ 'Yield')) %>%
  group_by(year, PlanType) %>%
  summarize(liab = sum(liab), prem=sum(prem), indem=sum(indem), 
            acres=sum(quantNet, na.rm=TRUE)) ->
sob_by_year_plan


g_acres <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = acres/1e6, x = year,  fill = PlanType), 
           data = sob_by_year_plan, stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Acres (millions)") +
  ggtitle("US MPCI Acres by Product Type")
g_acres 


g_prem <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = prem/1e9, x = year,  fill = PlanType), 
           data = sob_by_year_plan, stat="identity") +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Premium ($ B)") +
  ggtitle("US MPCI Premium by Product Type")

g_prem
