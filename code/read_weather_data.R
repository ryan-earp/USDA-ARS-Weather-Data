# Load libraries
library(tidyverse)
library(lubridate)
library(readxl)
library(chron)
library(readODS)

# Read in file

read_cell <- function(file_path, cell){
  
  range <- str_c(cell, ":", cell)
  
  cell <- read_excel(file_path,
                     range = range, 
                     col_names = FALSE) %>% 
    pull(...1)
  
  return(cell)
}

read_year <- function(file_path){

  year <- read_cell(file_path, "D3")

  return(year)
}


read_month <- function(file_path){
  
  month <- read_cell(file_path, "B3")

  return(month)
}

# Full Function 1st attempt (no worky)

import_wth <- function(data){
  wth_orig <- read_xlsx(data, col_names = FALSE, na = c("", "-")) 
  

  
   wth_data <- wth_orig [c(30:60), c(1:14)] 
    names(wth_data) <- c("day", "temp_maximum", "temp_minimum", "temp_range", "temp_set_max", 
             "precip_time_of_beginning", "precip_time_of_ending", "precip_amount",
             "precip_snowfall_in_inches", "precip_snow_depth_tobs",
             "wind_dir_tobs", "weather_state_tobs", "wind_dir_day", "weather_state_day")

month <- wth_orig$...2[3]  
year <- wth_orig$...4[3]  
location <- wth_orig$...6[3]
county <- wth_orig$...8[3] 
state <- wth_orig$...2[4]  
lat <- wth_orig$...4[4]
long <- wth_orig$...6[4]

wth_data %>% 
  mutate(date = mdy(paste(month, day, year)), 
         precip_time_of_beginning = format(times(as.numeric(precip_time_of_beginning))),
         precip_time_of_ending = ifelse(is.na(as.numeric(precip_time_of_ending)),
                                        precip_time_of_ending,
                                        format(times(as.numeric(precip_time_of_ending)))),
         location = rep((wth_orig$...6[3]), nrow(.)),
         county = rep(county, nrow(.)),
         state = rep(state, nrow(.)),
         lat = rep(lat, nrow(.)),
         long= rep(long, nrow(.))) %>%
  subset(select = -c(day)) %>% 
  select(date, location, county, state, lat, long, everything())
    
return(wth_data)
}

coerce_sky_condition <- function(condition){
  cond_list <- c("Cloudy", "Clear")
  
  clean_condition <- adist(condition, cond_list) %>% 
    apply(1, which.min) %>% 
    cond_list[.]
  
  return(clean_condition)
}

sanitize_year <- function(year){
  if(year == "2021"){
    year <- "1921"
  }
  return(year)
}

sanitize_month <- function(month){
  if(month %in% c("Fabruary", "Feburary")){
    month <- "February"
  }else if(month %in% c("Semptember", "Spet.")){
    month <- "September"
  }
  return(month)
}

# Full Function 2nd attempt (works minus some odd precip values and not sure how it handles months with dif number of days)
read_wth_data <- function(file_name){
  if(str_detect(file_name, "1937-11")){
    sheet = "Nov. 1937"
  }else{
    sheet = 1
  }
  if(str_detect(file_name, "1896-01")){
    cshift <- 1
  }else{
    cshift <- 0
  }
  if(str_detect(file_name, "\\.ods$")){
    raw <- read_ods(file_name, col_names = FALSE, sheet = sheet) %>% 
      as_tibble() %>% 
      mutate(across(where(is.numeric), .fns = as.character))
  }else{
    raw <- read_excel(file_name, col_names = FALSE, sheet = sheet)
  }
  wdata <- raw[30:60, 1:14] %>% 
    rename_with(.fn = ~c("day", "temp_maximum", "temp_minimum", "temp_range", "temp_set_max", 
                    "precip_time_of_beginning", "precip_time_of_ending", "precip_amount",
                    "precip_snowfall_in_inches", "precip_snow_depth_tobs",
                    "wind_dir_tobs", "weather_state_tobs", "wind_dir_day", "weather_state_day")) %>% 
    mutate(across(c(temp_maximum, temp_minimum, temp_range, temp_set_max,
                    precip_amount, precip_snowfall_in_inches,
                    precip_snow_depth_tobs),
                  list(num = as.numeric))) %>% 
    rename_with(c(temp_maximum, temp_minimum, temp_range, temp_set_max,
                  precip_amount, precip_snowfall_in_inches,
                  precip_snow_depth_tobs,
                  precip_time_of_beginning, precip_time_of_ending),
                .fn = ~str_c(.,"_orig")) %>% 
    rename_with(matches("_num$"),
                .fn = ~str_remove(.,"_num$")) %>% 
    mutate(year = sanitize_year(as.character(raw[3, 4 + cshift])),
           month = sanitize_month(as.character(raw[3, 2 + cshift])),
           observer = as.character(raw[64, 2]),
           date = mdy(paste(month, day, year)),
           precip_time_of_beginning = format(times(as.numeric(precip_time_of_beginning_orig))),
           precip_time_of_ending = ifelse(is.na(as.numeric(precip_time_of_ending_orig)),
                                          precip_time_of_ending_orig,
                                          format(times(as.numeric(precip_time_of_ending_orig))))) %>% 
    select(date, everything(), -c(day, month, year))
    
return(wdata)
}


