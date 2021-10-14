# Load libraries

library(tidyverse)
library(lubridate)
library(readxl)

# Read in file

year <- read_excel("../data/Weather_1940-01.xlsx",
                   range = "D3:D3", 
                   col_names = FALSE) %>% 
  pull(...1)

month <- read_excel("../data/Weather_1940-01.xlsx",
                    range = "B3:B3", 
                    col_names = FALSE) %>% 
  pull(...1)


cnames <- c("day", "Tmax", "Tmin", "Trange")

wdata <- read_excel("../data/Weather_1940-01.xlsx",
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
