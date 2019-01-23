library(tidyverse)
library(rvest)
library(magick)

about <- read_html("https://www.rstudio.com/about/")

about %>%
  html_nodes(".person-name") %>%
  html_text() ->
  rstudio_folks

about %>%
  html_nodes(".person-title") %>%
  html_text() ->
  rstudio_title_location

about %>%
  html_nodes(".fusion-clearfix") %>% # double nested
  html_nodes(".fusion-clearfix") %>%
  html_text() %>%
  gsub("^\\s+|\\s+$", "", .) ->  #strip white space
  rstudio_descriptions

# all links associated with people
about %>%
  html_nodes(".fusion-clearfix") %>% # double nested
  html_nodes(".fusion-clearfix") %>%
  map(. %>% html_nodes("a")) ->
  links

about %>%
  html_nodes(".fusion-clearfix") %>%
  html_nodes(".person-desc") %>%
  map(. %>% html_nodes(".fusion-icon-twitter"))  %>%
  map(. %>% html_attr("href"))  ->
  twitter_links

about %>%
  html_nodes(".fusion-clearfix") %>%
  html_nodes(".person-desc") %>%
  map(. %>% html_nodes(".fusion-icon-linkedin"))  %>%
  map(. %>% html_attr("href"))  ->
  linkedin_links

### start selenium docker image
system("docker run -d -p 4445:4444 selenium/standalone-firefox:2.53.0")

library('RSelenium')
remDr <- remoteDriver(remoteServerAddr = "localhost",
                      port = 4444L)
remDr$open()

# log into linkedin
remDr$navigate("https://www.linkedin.com/")
remDr$getTitle()
remDr$screenshot(display = TRUE)
loginemail <- remDr$findElement("css selector", "#login-email")
loginemail$sendKeysToElement (list(Sys.getenv("LI_ID")))
loginpwd <- remDr$findElement("css selector", "#login-password")
loginpwd$sendKeysToElement (list(Sys.getenv("LI_PWD"),
                                 "\uE007"))
remDr$screenshot(display = TRUE)

remote_driver <- remDr

get_linkedin_pic <- function(remote_driver = remDr, url, outfile) {
  Sys.sleep(rlnorm(1, .6, .2))
  remote_driver$navigate(url)
  #remote_driver$screenshot(display = TRUE)
  pic <- remote_driver$findElement("css selector", "#ember62")
  pic$clickElement()
  
  #resize window & screenshot
  remote_driver$setWindowSize(200, 350)
  #remote_driver$screenshot(display = TRUE)
  
  tmpfile <- tempfile()
  remote_driver$screenshot(file = tmpfile)
  remote_driver$setWindowSize(1024, 768)
  
  #remote_driver$screenshot(display = TRUE)
  
  Sys.sleep(rlnorm(1, .6, .2))
  remote_driver$mouseMoveToLocation(x = 5, y = 5)
  remote_driver$buttondown()
  remote_driver$buttonup()
  Sys.sleep(rlnorm(1, .6, .2))
  #remote_driver$screenshot(display = TRUE)
  
  image_read(tmpfile) %>%
    image_crop(geometry_area(
      width = 285,
      height = 280,
      x_off = 0,
      y_off = 0
    )) ->
    resized
  print(resized)
  image_write(resized, outfile)
}


get_linkedin_pic(remDr, url, outfile)

linkedin_links %>%
  str_split(., "/") %>%
  pluck(5)

linkedin_links %>%
  str_split(., "/", simplify = TRUE) -> tst


linkedin_links %>%
  str_split("/") %>%
  map2_chr(pmin(5, lengths(.)), pluck)









library(rtweet)

create_token(
  app = "cmastication_twitter_analysis",
  consumer_key = "xOBo64J1I4tLhmAPxe9MSmNgb",
  consumer_secret = "0pAuKBTW6SKYlCqCQVLzRPRVvzJkcNAb8A2HCnByv0hXygKDKM")

## search for 18000 tweets using the rstats hashtag
rstudioconf <- search_tweets(
  "#rstudioconf", n = 18000, include_rts = FALSE
)

ts_plot(rstudioconf, "6 hours") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold")) +
  labs(
    x = NULL, y = NULL,
    title = "Frequency of #rstudioconf tag",
    subtitle = "Twitter status (tweet) counts aggregated using siz-hour intervals",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

