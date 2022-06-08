pacman::p_load(tidyverse, readxl, simplevis, lubridate)

# Import file
url <- "https://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls"
destfile <- "pswrgvwall.xls"
curl::curl_download(url, destfile)
gasdata <- read_excel(destfile, skip = 2, sheet = 2)
gasdata <- janitor::clean_names(gasdata)

# Explore

## New England, Regular gas prices from 2010
newenglanddata <- gasdata %>% mutate(new_england_regular = weekly_east_coast_regular_conventional_retail_gasoline_prices_dollars_per_gallon) %>%
  filter(date > "2010-01-01") %>% select(date, new_england_regular)

daterange <- paste(min(newenglanddata$date), max(newenglanddata$date), sep = " - ")

newenglanddata %>% simplevis::gg_line(
  x_var = date,
  x_labels = scales::date_format("%Y"),
  y_var = new_england_regular,
  y_labels = scales::dollar_format(),
  y_title = "Price of Regular Gasoline",
  title = "Price of Regular Gasoline",
  subtitle = paste0("in New England from ", daterange)
)

## Zoom into 2022
eastcoastdata <- gasdata %>% mutate(eastcoast_regular = weekly_east_coast_regular_conventional_retail_gasoline_prices_dollars_per_gallon) %>%
  filter(date > "2022-01-01") %>% select(date, eastcoast_regular)

daterange <- paste(min(eastcoastdata$date), max(eastcoastdata$date), sep = " - ")

eastcoastdata %>% simplevis::gg_line(
  x_var = date,
  x_labels = scales::date_format(),
  y_var = eastcoast_regular,
  y_labels = scales::dollar_format(),
  y_title = "Price of Regular Gasoline",
  title = "Price of Regular Gasoline",
  subtitle = paste0("in New England from ", daterange)
)

## Compare w/other regions
regiondata <-
  gasdata %>% mutate(
    us = weekly_u_s_regular_conventional_retail_gasoline_prices_dollars_per_gallon,
    eastcoast = weekly_east_coast_regular_conventional_retail_gasoline_prices_dollars_per_gallon,
    midwest = weekly_midwest_regular_conventional_retail_gasoline_prices_dollars_per_gallon,
    gulfcoast = weekly_gulf_coast_regular_conventional_retail_gasoline_prices_dollars_per_gallon,
    rockies = weekly_rocky_mountain_regular_conventional_retail_gasoline_prices_dollars_per_gallon,
    westcoast = weekly_west_coast_regular_conventional_retail_gasoline_prices_dollars_per_gallon,
    date = as.Date(date)
  ) %>%
  select(date, us:westcoast) %>%
  pivot_longer(cols = !date,
               names_to = "region",
               values_to = "price") %>%
  drop_na()

regiondata %>%
  filter(date > "2021-12-31") %>%
  gg_line_col(date,
              price,
              region,
              size_line = 1,
              size_point = 0,
              alpha_point = 0,
              theme = theme_light(),
              y_labels = scales::dollar_format(),
              col_labels = c("East Coast",
                             "Gulf Coast",
                             "Midwest",
                             "Rockies",
                             "US",
                             "West Coast"),
              y_title = "Price of Regular Gasoline",
              title = "Price of Regular Gasoline",
              subtitle = paste0("Regionally from ", daterange))

# Forecast
set.seed(12345)
pacman::p_load(tsibble, fable)

forecastdata <- gasdata %>%
  filter(date > "2021-12-31") %>%
  select(date, price = weekly_east_coast_regular_conventional_retail_gasoline_prices_dollars_per_gallon) %>%
  mutate(index = as_date(date)) %>%
  as_tsibble(index = index)

forecastdata %>% gg_line(index, price)

forecastdata %>% tail(5)

forecastdata %>%
  model(TSLM(formula = price ~ trend())) %>%
  forecast(bootstrap = T)
