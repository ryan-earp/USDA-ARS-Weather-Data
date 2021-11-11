source("code/read_weather_data.R")

 wth_data <- list.files("data/raw/Weather Record",
                        "^[0-9]{4}$",
                        full.names = TRUE) %>% 
   map(~list.files(., "\\.xlsx", full.names = TRUE)) %>% 
   unlist() %>% 
   tibble(file_path = .) %>% 
   mutate(imported_data = map(file_path, ~try(read_wth_data(.))))
 
 wth_data %>% 
   filter(map_lgl(imported_data, ~{"try-error" %in% class(.)})) %>% 
   pull(imported_data)
 