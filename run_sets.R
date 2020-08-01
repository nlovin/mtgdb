# load packages
library(tidyverse)

# load function
source("get_price_history.R")

# run sets
run_sets <- mtg::mtg("sets")[["sets"]] %>% 
  as_tibble() %>% 
  select(code:type,date=releaseDate,online=onlineOnly) %>% 
  unnest(cols = c(code, name, type, date, online)) %>% 
  filter(type == "expansion" | type == "core") %>% 
  filter(!str_detect(name, "Foreign Black")) %>% 
  mutate(date = lubridate::as_date(date)) %>% 
  filter(date > lubridate::as_date(11422)) %>% 
  filter(code != "M10") %>% 
  filter(name != "Guilds of Ravnica") %>% 
  filter(name != "Alara Reborn") %>% 
  mutate(name = ifelse(name == "Magic 2014", "Magic 2014 Core Set", name),
         name = ifelse(name == "Magic 2015", "Magic 2015 Core Set", name),
         name = ifelse(name == "Time Spiral Timeshifted", "Timeshifted", name)) %>% 
  pull(name)

# run masters sets
# run_sets <- mtg::mtg("sets")[["sets"]] %>% 
#   as_tibble() %>% 
#   select(code:type,date=releaseDate,online=onlineOnly) %>% 
#   unnest(cols = c(code, name, type, date, online)) %>% 
#   filter(type == "masters") %>% 
#   filter(online==F) %>% 
#   filter(name != "Rinascimento") %>% 
#   mutate(date = lubridate::as_date(date)) %>% 
#   filter(date > lubridate::as_date(11422))


for (set in run_sets) {
  get_set_prices(set)
  
  
  t_run <- runif(1,100,300)
  print(paste0("Sleeping for ", t_run, " seconds"))
  Sys.sleep(t_run)
} 
