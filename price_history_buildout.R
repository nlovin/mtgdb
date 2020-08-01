library(tidyverse)
library(httr)
library(rvest)



###### get single card price history -----
url <- "https://www.mtggoldfish.com/price/Magic+2010/Goblin+Piker#paper"
pg2 <- GET(url)

c <- content(pg2, as = 'text')


str_squish(c) -> s

str_extract(s, 'price-sources-paper"\\)\\.toggle\\(true\\).+containing div') %>% 
  str_remove_all('\\+= \\"\\\\n') %>% 
  str_remove_all('\\"') %>% 
  str_remove_all("g = new Dygraph.+") %>% 
  str_remove_all(" d ") %>% 
  str_split(";") %>% 
  unlist() -> tst

nme <- tst[2] %>% 
  str_remove(" var= Date,")

tst[-1:-2] %>% 
  str_remove_all(" d ") %>% 
  str_split(",", simplify = T) %>% 
  as_tibble(.name_repair = "minimal") %>% 
  rename(date = 1, price = 2) %>% 
  filter(date != "") %>% 
  mutate(price = as.numeric(price),
         card_name = nme) %>% 
  filter(!is.na(price)) %>% 
  mutate(date = lubridate::as_date(date)) -> tmp

# alt way without str_squish, but can't separate paper from mtgo
# str_extract_all(c, "\\+=.*") %>% 
#   unlist() %>% 
#   str_remove_all('\\+= \\"\\\\n') %>% 
#   str_remove_all('\\";') %>% 
#   str_split(", ",simplify = T) %>% 
#   as_tibble(.name_repair = "minimal") %>% 
#   rename(date = 1, price = 2) -> tst


###### create all card urls for given set -----

# url format
# https://www.mtggoldfish.com/price/Magic+2010/Protean+Hydra#paper

library(DBI)
library(RSQLite)

# tbl <- dbReadTable(DBI::dbConnect(RSQLite::SQLite(), 
#                                   "data/mtg_db.sqlite"),
#                    "edhcards") %>% 
#   as_tibble()
# 
# dbDisconnect(DBI::dbConnect(RSQLite::SQLite(), "data/mtg_db.sqlite"))


m10_urls <- tbl %>% 
  filter(set_code == "m10") %>% 
  select(name) %>% 
  mutate(name = str_remove_all(name, "'"),
         name = str_replace_all(name, " ", "+"),
         name = paste0("https://www.mtggoldfish.com/price/Magic+2010/",
                       name,
                       "#paper")) %>% 
  pull(name)



###### Loop through m10 to get price histories ------

m10_hist <- tibble()

for (card in m10_urls[4:length(m10_urls)]) {
  
  s <- str_squish(content(GET(card), as = 'text'))
  
  str_extract(s, 'price-sources-paper"\\)\\.toggle\\(true\\).+containing div') %>% 
    str_remove_all('\\+= \\"\\\\n') %>% 
    str_remove_all('\\"') %>% 
    str_remove_all("g = new Dygraph.+") %>% 
    str_remove_all(" d ") %>% 
    str_split(";") %>% 
    unlist() -> tmp
  
  nme <- tmp[2] %>% 
    str_remove(" var= Date,")
  
  tmp[-1:-2] %>% 
    str_remove_all(" d ") %>% 
    str_split(",", simplify = T) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    rename(date = 1, price = 2) %>% 
    filter(date != "") %>% 
    mutate(price = as.numeric(price),
           card_name = nme) %>% 
    filter(!is.na(price)) %>% 
    mutate(date = lubridate::as_date(date)) -> tmp
  
  m10_hist <- bind_rows(m10_hist, tmp)
  
  print(paste0("Finished with ", card))
  t <- runif(1,3,6)
  print(paste0("Sleeping for ", t, " seconds"))
  Sys.sleep(t)
}
# add name
m10_hist <- m10_hist %>% 
  mutate(set_code = "m10",
         set_name = "Magic 2010")


# write to DB
db <- dbConnect(SQLite(), "data/mtg_db.sqlite")

dbWriteTable(db, "price_history", m10_hist)

dbReadTable(db, "price_history") %>%
  head(10)

dbDisconnect(db)



#### set unique names ----
tbl %>% 
  select(set_name) %>% 
  distinct()