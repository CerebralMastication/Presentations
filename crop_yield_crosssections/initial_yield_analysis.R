library(tidyverse)
library(ggthemes)
library(scales)

## read in summary of business data from csv
sob_by_state_year_plan <- read_csv('./data/sob_by_state_year_plan.csv')


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
sob_by_state_year_plan <- read_csv('./data/sob_by_state_year_plan.csv')


years <- 1989:2017
graph_list <- lapply(years, plot_corn_prices)

list_to_plot <- graph_list[c(20, 24, 27, 28)]
library(gridExtra)
do.call("grid.arrange", c(list_to_plot, ncol=2))



