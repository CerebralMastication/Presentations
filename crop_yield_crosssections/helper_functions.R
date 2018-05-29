

## functions that we'll use later... load this first.


### helper function to plot a year price change data from files downloaded 
### in the compile_data.R script

plot_corn_prices <- function(year) {
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
  
  price_change <- paste0(round(price_change[[1]], digits=2) * 100, '%')
  
  ## shade spring and harvest price - repeats logic from above but in 
  ## a different format. Should not have to repeat, but can't make it work
  ## otherwise
  rects <- data.frame(startDate = as.Date(c(paste0(year, '-02-01'),paste0(year, '-10-01'))), 
                      endDate = as.Date(c(paste0(year, '-02-28'),paste0(year, '-10-31'))), 
                      col = c('S','H'))
  
  g_price <- ggplot() + 
    theme_economist(base_size = 65) + 
    scale_fill_economist() +
    geom_rect(data = rects,  #bring in shading rectangles
              aes(xmin = startDate, xmax = endDate, 
                  ymin = -Inf, ymax = Inf, 
                  fill = col), 
              alpha = 0.4) +
    geom_line(aes(y = Settle/100  , x = Date),  # plot settle price
              data = year_data, stat="identity", 
              size=4) +
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
