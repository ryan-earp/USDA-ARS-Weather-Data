source("code/read_weather_data.R")

 wth_data <- list.files("data/raw/Weather Record",
                        "^[0-9]{4}$",
                        full.names = TRUE) %>% 
   map(~list.files(., "(\\.xlsx)|(\\.ods)", full.names = TRUE)) %>% 
   unlist() %>% 
   str_subset("(1893_04\\.xlsx)|(~\\$)", negate = TRUE) %>% 
   tibble(file_path = .) %>% 
   mutate(imported_data = map(file_path, ~try(read_wth_data(.)))) %>% 
   unnest(imported_data) %>% 
   arrange(date)
 
wth_data %>%
   filter(!is.na(date)) %>% 
   arrange(date) %>% 
   write_csv("data/processed/imported_weather_data_1893-1940.csv")
 
