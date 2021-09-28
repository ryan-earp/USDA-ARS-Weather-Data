
# load libraries ----------------------------------------------------------

library(readxl)
library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(chron)
library(lubridate)
library(ggplot2)
library(tidyr)



# read data and clean column names ----------------------------------------

w1940_1 <- read_xlsx("./data/raw/Weather_1940-01.xlsx",
                   col_names = FALSE,
                   na = c("", "-")) %>%
  clean_names()


# examine and build header names ------------------------------------------

#create header1 vector
header1_start <- which(str_detect(w1940_1$x1, "\\bDate\\b") 
                 & str_detect(w1940_1$x2, "\\bTemperature\\b"))
header1 <- w1940_1[header1_start, ] %>%
  make_clean_names() %>%
  str_replace("na", NA_character_)

#create header2 vector
header2_start <- which(str_detect(w1940_1$x2, "\\bmaximum\\b") 
                         & str_detect(w1940_1$x3, "\\bminimum\\b"))

header2 <- w1940_1[header2_start, ] %>%
  make_clean_names() %>%
  str_replace("na", NA_character_)

# for comparison
headers <- tibble("header1" = header1,
                  "header2" = header2)

# construct header vector
my_headers <- c(headers$header1[1],
                paste0("temp_", headers$header2[2:5]),
                paste0("precip_", headers$header2[6:9]),
                "precip_snow_depth_tobs",
                "wind_dir_tobs",
                "weather_state_tobs",
                "wind_dir_day",
                "weather_state_day")


# create data subset ------------------------------------------------------

data_begin <- header2_start + 1
data_end <- which(str_detect(w1940_1$x1, "1\\*")) - 1

w1940_dat <- w1940_1 %>%
  slice(data_begin:data_end)
names(w1940_dat) <- my_headers

# remove unecessary values
rm(header1, header2, headers, my_headers, header1_start, header2_start)


# create date/time variable for precip ------------------------------------

month <- w1940_1$x2[3]
year <- w1940_1$x4[3]
location <- w1940_1$x6[3]
county <- w1940_1$x8[3]
state <- w1940_1$x2[4]
lat <- w1940_1$x4[4]
long <- w1940_1$x6[4]

# add date value and convert decimal time back to hour:minute
w1940_dat_datetime <- w1940_dat %>%
  mutate(date_conv = mdy(paste(month, date, year)),
    precip_time_of_beginning_conv = format(times(as.numeric(precip_time_of_beginning))),
    location = rep(location, nrow(.)),
    county = rep(county, nrow(.)),
    state = rep(state, nrow(.)),
    lat = rep(lat, nrow(.)),
    long= rep(long, nrow(.)))

# for non-NA time_conv values, add date for parsing
w1940_dat_datetime$precip_time_of_beginning_conv[!is.na(w1940_dat_datetime$precip_time_of_beginning_conv)] <- paste(w1940_dat_datetime$date_conv[!is.na(w1940_dat_datetime$precip_time_of_beginning_conv)], w1940_dat_datetime$precip_time_of_beginning_conv[!is.na(w1940_dat_datetime$precip_time_of_beginning_conv)])

# coerce to date object
w1940_dat_datetime$precip_time_of_beginning_conv <- ymd_hms(w1940_dat_datetime$precip_time_of_beginning_conv, tz = "US/Central")


write_csv(w1940_dat_datetime, "./data/processed/w1940_dat_datetime_20210901.csv")

# plot snowfall depth across month ----------------------------------------

w1940_1_snow_plot <- w1940_dat_datetime %>%
  drop_na(precip_snowfall_in_inches)

snowfall_plot <- ggplot(w1940_1_snow_plot) +
  geom_line(aes(x = date_conv,
                y = precip_snowfall_in_inches,
                group = 1)) +
  theme(legend.position = 'bottom',
        panel.grid.minor.x = element_blank(),
        panel.grid.major = element_line(color = '#BABABA')) +
  labs(x = "Date",
       y = "Snowfall in inches",
       title = "Snowfall in inches, January 1940 Stillwater Station", 
       caption = 'Source: U. S. Department of Agriculture, Weather Bureau')

snowfall_plot

ggsave("./fig/194001_snowfall.png", snowfall_plot)

  
