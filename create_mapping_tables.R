##################### JOINS AND KEYS --------------------
library(tidyverse)
library(httr)
library(rvest)
library(DBI)
library(RSQLite)
library(RPostgres)


con <- dbConnect(RPostgres::Postgres()
                 , host='localhost'
                 , port='5432'
                 , dbname='mtgdb'
                 , user='postgres'
                 , password=ifelse(exists('pw'),
                                   pw,
                                   askpass::askpass())
)

tbl <- dbReadTable(con, "cards") %>% 
  as_tibble()
t1<-Sys.time()
price <- dbReadTable(con, "price_history") %>% 
  as_tibble()
t2<-Sys.time()
print(t2-t1);rm(t1,t2)
dbDisconnect(con)




tbl %>% 
  select(mvid,
         name,
         set_name,
         set_code,
         is_split,
         main,
         setcard_id) %>% 
  filter(main == 1) %>% 
  mutate(og_name = name) %>% 
  mutate(name = ifelse(str_detect(name, "/") & is_split==F,
                       str_remove_all(name, " //.+"),
                       name),
         name = str_remove_all(name, ",")) -> map_cards


price %>% 
  select(name,set_name,set_code) %>% 
  distinct() -> map_price



full_map <- full_join(map_cards, 
                      map_price, 
                      by = c("name",
                             "set_code",
                             "set_name")) %>% 
  select(-og_name, -is_split, -main)

price_join <- left_join(price, full_map, 
                        by= c("name",
                              "set_code",
                              "set_name")) %>% 
  distinct()

price_tbl <- price_join %>% 
  select(date, price, setcard_id)
