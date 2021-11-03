# Load libraries

library(tidyverse)
library(lubridate)
library(readxl)
library(chron)

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

year <- read_excel("../data/Weather_1940-01.xlsx",
                   range = "D3:D3", 
                   col_names = FALSE) %>% 
  pull(...1)

month <- read_excel("../data/Weather_1940-01.xlsx",
                    range = "B3:B3", 
                    col_names = FALSE) %>% 
  pull(...1)


cnames <- c("day", "Tmax", "Tmin", "Trange")


wdata <- read_excel("data/raw/Weather_1940-01.xlsx",
                    col_names = FALSE)

wdata$...2

wdata <- read_excel("data/raw/Weather_1940-01.xlsx",
                    range = "A30:D60",
                    col_names = cnames) %>%
  mutate(year = year,
         month = month)

wdata %>% 
  ggplot(aes(x = day, y = Tmax))+
  geom_line()+
  geom_line(aes(y = Tmin), color = "orange")

wdata %>% 
  filter(Tmax - Tmin != Trange)


## Import entire excel file
## Start function here

wth_import <- read_xlsx("data/raw/Weather_1940-01.xlsx",
                        col_names = FALSE,
                        na = c("", "-"))

## Subset data section only and replace column names with correct names

wth_data <- wth_import [c(30:60), c(1:14)] 

headers <- c("day", "temp_maximum", "temp_minimum", "temp_range", "temp_set_max", 
             "precip_time_of_beginning", "precip_time_of_ending", "precip_amount",
             "precip_snowfall_in_inches", "precip_snow_depth_tobs",
             "wind_dir_tobs", "weather_state_tobs", "wind_dir_day", "weather_state_day")

names(wth_data) <- headers

## Create month/year variables and site specific information

month <- wth_import$...2[3]
year <- wth_import$...4[3]
location <- wth_import$...6[3]
county <- wth_import$...8[3]
state <- wth_import$...2[4]
lat <- wth_import$...4[4]
long <- wth_import$...6[4]

## Create new data set with full date and site specific info

wth_data_time <- wth_data %>% 
  mutate(date = mdy(paste(month, day, year)), 
         precip_time_of_beginning = format(times(as.numeric(precip_time_of_beginning))),
         precip_time_of_ending = ifelse(is.na(as.numeric(precip_time_of_ending)),
                                        precip_time_of_ending,
                                        format(times(as.numeric(precip_time_of_ending)))),
         location = rep(location, nrow(.)),
         county = rep(county, nrow(.)),
         state = rep(state, nrow(.)),
         lat = rep(lat, nrow(.)),
         long= rep(long, nrow(.))) %>%
  subset(select = -c(day))

## Possibilities to coerce precip begin and end times to include character values when needed

precip_time_of_ending = (if(is.numeric(precip_time_of_ending)){
  precip_time_of_ending = format(times(as.numeric(precip_time_of_ending)))
} else {precip_time_of_ending = precip_time_of_ending})

wth_data_time <- wth_data %>% 
  mutate(precip_time_of_ending = ifelse(is.numeric(precip_time_of_ending),
                               precip_time_of_ending = format(times(as.numeric(precip_time_of_ending))),
                               precip_time_of_ending = precip_time_of_ending))  
    
   
## Reorder columns into more logical format
wth_data_time %>% 
  select(date, location, county, state, lat, long, everything())

## Return wth_data_time and end function

date_loc <- function(data){
  data %>% 
    mutate(date = mdy(paste(month, day, year)), 
           precip_time_of_beginning = format(times(as.numeric(precip_time_of_beginning))),
           precip_time_of_ending = ifelse(is.na(as.numeric(precip_time_of_ending)),
                                          precip_time_of_ending,
                                          format(times(as.numeric(precip_time_of_ending)))),
           location = rep(location, nrow(.)),
           county = rep(county, nrow(.)),
           state = rep(state, nrow(.)),
           lat = rep(lat, nrow(.)),
           long= rep(long, nrow(.))) %>%
    subset(select = -c(day))
}

wth_time <- date_loc(data = wth_data)


# Full Function







