# load libraries ----------------------------------------------------------

library(readxl)
library(stringr)
library(janitor)
library(chron)
library(lubridate)
library(tidyverse)


# Read file 
wth_data <- read_csv("data/processed\\imported_weather_data_1893-1940.csv")

wth_data %>% 
  select(wind_dir_day) %>% 
  summary()

  pull(wind_dir_day) %>%
  unique()

wth_data %>% 
  mutate(trange_diff = temp_maximum-temp_range) %>% 
  filter(trange_diff !=0) %>% 
  ggplot(aes(x=trange_diff)) +
  geom_boxplot()

coerce_sky <- function(condistion){
  cond_list <- c("cloudy", "clear")
  adists(condition, cond_list) %>% 
    apply(1, which.min) %>% 
  cond_list
  
}

wth_data_precip <- wth_data %>% 
  select(date, precip_amount, precip_amount_orig) %>% 
  filter(precip_amount != precip_amount_orig)
  
wth_data_precip %>% 
  ggplot()+
  geom_line(aes(x = date, y = precip_amount)) +
  geom_line(aes(x = date, y = precip_amount_orig))

drop_na()
         
         
wth_data %>% 
  select()

head(wth_data)


precip_amount_orig

wth_data %>% 
  ggplot(aes(x = date, y = precip_amount))+
  geom_line()




  
  