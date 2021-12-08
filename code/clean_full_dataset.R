# load libraries ----------------------------------------------------------

library(readxl)
library(stringr)
library(janitor)
library(chron)
library(lubridate)
library(tidyverse)


# Read file 
wth_data <- read_csv("data/processed\\imported_weather_data_1893-1940.csv")


# Precipitation Cleanup

wth_data_precip_dif <- wth_data %>% 
  select(date, precip_amount, precip_amount_orig) %>% 
  filter(precip_amount != precip_amount_orig)

## High values of rainfall
wth_data %>% 
  ggplot()+
  geom_line(aes(x = date, y = precip_amount)) 


precip <- wth_data %>% 
  select(date, precip_amount) %>% 
  drop_na() %>% 
  arrange(desc(precip_amount))
## Four values are large amounts that probably are not right


# Character of day
bad_states <- wth_data %>% 
  select(weather_state_day) %>%
  unique() 

wth_data %>% 
  group_by(weather_state_day) %>% 
  ggplot(aes(x = weather_state_day)) +
  geom_bar()

## 49 different variables, most of them are misspellings but some are extra variables
  
  
  
# Wind Direction for day
wind_dir <- 
  wth_data %>% 
  select(date, wind_dir_day) %>% 
  filter(wind_dir_day == "?")

  unique()

  pull(wind_dir_day) %>%
  

    
    wind_dir <- 
    wth_data %>% 
    select(date, wind_dir_tobs) %>% 
    filter(wind_dir_tobs == "SN")    
    
    
str_replace_all(wind_dir, c(" //$"))  
  #in read weather data file, make a function and add it to the original functions
    
  
# Class Examples
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
         


wth_data %>% 
  colnames()

  
  