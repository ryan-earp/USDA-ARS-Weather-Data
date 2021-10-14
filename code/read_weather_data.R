# Load libraries

library(tidyverse)
library(lubridate)
library(readxl)

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
