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

  
  