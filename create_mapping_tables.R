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



price_tbl <- left_join(price, full_join(tbl %>% 
                                           select(mvid,
                                                  name,
                                                  set_name,
                                                  set_code,
                                                  is_split,
                                                  main,
                                                  setcard_id,
                                                  card_id) %>% 
                                           filter(main == 1) %>% 
                                           mutate(og_name = name) %>% 
                                           mutate(name = ifelse(str_detect(name, "/") & is_split==F,
                                                                str_remove_all(name, " //.+"),
                                                                name),
                                                  name = str_remove_all(name, ",")), 
                                         price %>% 
                                           select(name,set_name,set_code) %>% 
                                           distinct(), 
                                         by = c("name",
                                                "set_code",
                                                "set_name")) %>% 
                          select(-og_name, -is_split, -main), 
                        by= c("name",
                              "set_code",
                              "set_name")) %>% 
  distinct() %>% 
  select(date, price, setcard_id, card_id)



con <- dbConnect(RPostgres::Postgres()
                 , host='localhost'
                 , port='5432'
                 , dbname='mtgdb'
                 , user='postgres'
                 , password=ifelse(exists('pw'),
                                   pw,
                                   askpass::askpass())
)

# this is just keep a backup copy of the original price table
db <- dbConnect(SQLite(), "data/mtg_db.sqlite")
dbWriteTable(db, "price_history", price, overwrite=T)
dbDisconnect(db)

# write the the new mapped price table
dbWriteTable(con, "price_history", price_tbl, overwrite=T)
dbDisconnect(con)





############### XXX ------------

card_id_tbl <- dta %>% 
  select(setcard_id, id, mvid)









################ Build out ----------------

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
