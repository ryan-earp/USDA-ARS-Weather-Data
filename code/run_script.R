source("code/read_weather_data.R")

 wth_data <- list.files("data/raw/Weather Record",
                        "^[0-9]{4}$",
                        full.names = TRUE) %>% 
   map(~list.files(., "(\\.xlsx)|(\\.ods)", full.names = TRUE)) %>% 
   unlist() %>% 
   str_subset("(1893_04\\.xlsx)|(~\\$)", negate = TRUE) %>% 
   tibble(file_path = .) %>% 
   mutate(imported_data = map(file_path, ~try(read_wth_data(.))))
 
 wth_data <- wth_data %>% 
   unnest(imported_data)
 
 wth_data %>% 
    mutate(wdt_type = map_chr(imported_data, ~class(.$wind_dir_tobs))) %>% 
    filter()
 
 wth_data %>% 
   filter(map_lgl(imported_data, ~{"try-error" %in% class(.)})) %>% 
   pull(imported_data)
 