library(tidyverse)
library(ggthemes)
library(scales)
library(gridExtra)

## read in summary of business data from csv
sob_by_state_year_plan <- read_csv('./data/sob_by_state_year_plan.csv', 
                                   col_types = cols(year = col_integer(), PlanType = col_character(), 
                                                    stAbbr = col_character(), liab = col_double(), 
                                                    prem = col_double(), indem = col_double(), 
                                                    acres = col_double() ) )
sob_by_state_year_plan %>% 
  filter(year <= 2017) ->
  sob_by_state_year_plan

## US MPCI Acres by Product Type (Revenue vs. Yield)
g_acres <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = acres/1e6, x = year,  fill = PlanType), 
           data = sob_by_state_year_plan, stat="identity") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Acres (millions)") +
  ggtitle("US MPCI Acres by Product Type")
g_acres 

## same as above but premium on the Y axis
g_prem <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_bar(aes(y = prem/1e9, x = year,  fill = PlanType), 
           data = sob_by_state_year_plan, stat="identity")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Premium ($ B)") +
  ggtitle("US MPCI Premium by Product Type")
g_prem

## kick out the state grouping then plot loss ratio over time by product type
sob_by_state_year_plan %>%
  group_by(year, PlanType) %>% #changed this grouping is all we do here
  summarize(liab = sum(liab), prem=sum(prem), indem=sum(indem), 
            acres=sum(acres, na.rm=TRUE)) ->
sob_by_state_year_plan_no_state

# now plot loss ratio over time by group
g_lr <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_line(aes(y = indem / prem , x = year,  color = PlanType), 
           data = sob_by_state_year_plan_no_state, stat="identity")+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Loss Ratio (%)") +
  ggtitle("US MPCI Loss Ratio by Product Type")
g_lr

## same as above but only IL
g_lr_il <- ggplot() + theme_economist() + scale_fill_economist() +
  geom_line(aes(y = (indem/prem) * 100 , x = year,  color = PlanType), 
            data = filter(sob_by_state_year_plan, stAbbr=='IL'), stat="identity")+
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
  filter(year %in% c(2015, 2016, 2012)) -> # 2016-g 2015-m 2012-b
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
  filter(year %in% c(2002, 2005, 2012)) ->
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


### same thing but with APH & REV
sob %>% 
  filter(quantType == 'Acres',        # field crops only 
         stAbbr == 'IL', cropName == 'CORN', 
         covLevel > .5) %>%          # kick out the low cover levels
  mutate(PlanType = case_when( planCd %in% c('25','42','44','45','73', '02', '03', '05', '06') ~ 'Revenue', 
                               TRUE ~ 'Yield')) %>%
  group_by(year,covLevel, PlanType) %>% 
  summarize(claim_rate = sum(indemCount) / sum(unitsReportingPrem), 
            prem = sum(prem), 
            indem = sum(indem), 
            lr = sum(indem) / sum(prem)) %>%
  filter(year %in% c(2002, 2005, 2012)) ->
  claims_rate_covlevel_plan_type

## for the 3 years we selected above let's plot claim rate vs cover level
## grouped by year so we can better understand the shape
g_cov_level <- ggplot(claims_rate_covlevel_plan_type) + theme_economist() + scale_fill_economist() + 
  facet_wrap(~ factor(PlanType, levels=c('Yield','Revenue'))) +
  aes(y = claim_rate*100 , x = round(covLevel,2), 
      color = as.character(year)) +
  geom_point(stat="identity") +
  geom_line(stat="identity") +
  theme(legend.position="bottom", 
        legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Coverage Level", y="Claim Rate (%)", 
       title = "Yield Protection: Claim Rate by Coverage Level") +
  scale_x_continuous(labels=function(x) sprintf("%.2f", x))
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

corn_prices <- read_csv('./data/corn_prices.csv')

g_corn_price <- ggplot() + 
  theme_economist() + 
  scale_fill_economist() + 
  scale_colour_economist() +
  geom_bar(aes(y = price_change , x = year), 
            data = filter(corn_prices, year <=2008), 
           stat="identity" , fill=ggthemes_data$economist$fg[['blue_gray']]) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  +
  theme(legend.position="bottom", legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="% Price Change Spring vs. Harvest") +
  ggtitle("CBOT December Contract Corn Prices")
g_corn_price

## superimpose yield changes over that image.... 
il_corn_yield <- read_csv( './data/il_corn_yield.csv')
il_corn_yield %>%
  filter(YEAR >= min(corn_prices$year) &
         YEAR <= 2008) ->
il_corn_yield

g_corn_price +
  geom_crossbar(data=il_corn_yield, aes(x=YEAR, y=yield_dev, 
                                        ymin=0, ymax=0) , 
                col=ggthemes_data$economist$fg[['red_dark']]
                ) +
  ggtitle("CBOT Corn and IL Yield Deviations")

years <- c( 1992, 1998, 2004, 2008)
graph_list <- lapply(years, plot_corn_prices)
do.call("grid.arrange", c(graph_list, ncol=2))


plot_corn_prices(2008)


## plot just IL corn yield with trend
ggplot(il_corn_yield) + 
  theme_economist() + 
  scale_fill_economist() + 
  scale_colour_economist() +
  aes(x=YEAR, y=plantedYield) +
  geom_line(col=ggthemes_data$economist$fg[['blue_gray']], 
            size = 2) +
  geom_line(aes(y=trend_yield), 
            col=ggthemes_data$economist$fg[['red_dark']], 
            size = 2) +
  theme(legend.position="bottom", 
        legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Yield") +
  ggtitle("IL Planted Corn Yield") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  


## plot detrended IL yield
ggplot(il_corn_yield) + 
  theme_economist() + 
  scale_fill_economist() + 
  scale_colour_economist() +
  aes(x=YEAR, y=yield_dev) +
  geom_line(col=ggthemes_data$economist$fg[['blue_gray']], 
            size = 2) + 
  geom_hline(yintercept = 0, 
             col=ggthemes_data$economist$fg[['red_dark']],
             size=2) +
  theme(legend.position="bottom", 
        legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Yield") +
  ggtitle("IL Planted Corn Yield") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  


# add boxplot
library(tidyverse)
library(ggthemes)
il_corn_yield <- read_csv('https://raw.githubusercontent.com/CerebralMastication/Presentations/master/crop_yield_crosssections/data/il_corn_yield.csv')

ggplot(il_corn_yield) + 
  theme_economist() + 
  scale_fill_economist() + 
  scale_colour_economist() +
  aes(x=YEAR, y=yield_dev) +
  geom_line(col=ggthemes_data$economist$fg[['blue_gray']], 
            size = 2) + 
  geom_hline(yintercept = 0, 
             col=ggthemes_data$economist$fg[['red_dark']],
             size=2) +
  geom_boxplot(aes(x      = YEAR,
                   middle = yield_dev,
                   lower  = yield_dev - .05,
                   upper  = yield_dev + .05,
                   ymin   = yield_dev - .15,
                   ymax   = yield_dev + .15),
               stat = "identity", 
               fill=ggthemes_data$economist$fg[['blue_light']] ) +
  theme(legend.position="bottom", 
        legend.direction="horizontal",
        legend.title = element_blank()) +
  labs(x="Year", y="Yield") +
  ggtitle("IL Planted Corn Yield") +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))  

  
  
  