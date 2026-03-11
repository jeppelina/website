library(haven)

# read stata dataset
data <- read_dta("/Users/jeppelina/Downloads/20060620_stata_datsets/analysis_sample.dta")

data <- data %>% filter(offender != 0) %>% 
  select(sale_date, amt_Price, distance, offender, offender_address_date)

# set variable indicating if offender address date is before or after sale date
data <- data %>% 
  mutate(offender_before_sale = ifelse(offender_address_date < sale_date, 1, 0))

# set years variable indicating number of years in whole years between sale date and offender address date
data <- data %>% 
  mutate(years_diff = as.numeric(difftime(sale_date, offender_address_date, units = "weeks")) / 52.25) %>%
  mutate(years_diff = floor(years_diff))
