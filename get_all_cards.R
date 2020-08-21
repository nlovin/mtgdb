library(httr)
library(tidyverse)

######## Get Bulk Card Data ------

tdy_file <-
  paste0("bulk_",
         lubridate::today() %>% str_replace_all("-", "_"),
         ".rds")
if (file.exists(paste0("data/bulk/",tdy_file))) {
  bulk_data <- read_rds(tdy_file)
} else{
  res_bulk <- jsonlite::fromJSON("https://api.scryfall.com/bulk-data/default-cards")
  bulk_data <- jsonlite::fromJSON(res_bulk$download_uri)
  file.remove(list.files("data/bulk",full.names = T))
  write_rds(bulk_data, paste0("data/bulk/",tdy_file))
}

# bulk_data <- read_rds("data/bulk.rds") %>% 
#   as_tibble()

### Data updated at
data_updated_at <- lubridate::as_date(res_bulk$updated_at)

### Store Password Locally ----
if (exists("pw")) {
  print("Password already stored")
}else{
pw <- askpass::askpass()
}
### Ask for server location
if (exists("host")) {
  print("Hostname already stored")
}else{
host_tmp <- svDialogs::dlg_input(message="Enter last 2-3 digits of the host name: ") %>% .$res

host <- ifelse(host_tmp=="localhost","localhost", paste0("192.168.1.",host_tmp) );rm(host_tmp)
}
######## Create Card Table ------
make_mvid <- . %>% 
  lapply(function(x) ifelse(is.null(x), NA, x)) %>% 
  flatten() %>% 
  unlist()

bulk_data %>% 
  filter(digital==F) %>% 
  mutate(mvid = multiverse_ids %>% 
           make_mvid()) %>% 
  mutate(price = prices %>% pull(usd),
         price_foil = prices %>% pull(usd),
         price_tix = prices %>% pull(tix),
         standard = legalities %>% pull(standard),
         commander = legalities %>% pull(commander),
         pauper = legalities %>% pull(pauper),
         modern = legalities %>% pull(modern),
         legacy = legalities %>% pull(legacy),
         pioneer = legalities %>% pull(pioneer),
         historic = legalities %>% pull(historic),
         vintage = legalities %>% pull(vintage),
         penny = legalities %>% pull(penny),
         set = str_to_upper(set),
         data_created_date = lubridate::today()) %>% 
  select(mvid,
         name,
         set_name,
         set_code = set,
         set_type,
         date_released = released_at,
         mana_cost:toughness,
         foil:variation, 
         collector_number:flavor_text,
         artist,
         loyalty, 
         hand_modifier,
         price:price_tix,
         standard:penny,
         data_created_date,
         scryfall_id = id) %>% 
  bind_cols(.,
            bulk_data %>% 
              filter(digital==F) %>% 
              select(color_identity) %>% 
              flatten() %>% 
              map( ~ tibble(color_id = .x %>% 
                              unlist() %>%  
                              paste(collapse = "|"))) %>% 
              reduce(bind_rows)) %>% 
  bind_cols(.,
            bulk_data %>% 
              filter(digital==F) %>% 
              select(colors) %>% 
              flatten() %>% 
              map( ~ tibble(colors = .x %>% 
                              unlist() %>%  
                              paste(collapse = "|"))) %>% 
              reduce(bind_rows)) %>% 
  bind_cols(.,
            bulk_data %>% 
              filter(digital==F) %>% 
              select(keywords) %>% 
              flatten() %>% 
              map( ~ tibble(keywords = .x %>% 
                              unlist() %>%  
                              paste(collapse = "|"))) %>% 
              reduce(bind_rows)) %>% 
  select(mvid,
         name,
         colors,
         color_id,
         set_name:artist,
         keywords,
         loyalty:price_tix,
         standard:penny,
         data_created_date,
         scryfall_id) %>% 
  rename(type = type_line,
         has_foil = foil,
         has_nonfoil = nonfoil,
         is_promo = promo,
         is_oversized = oversized,
         is_reprint = reprint) %>% 
  filter(set_type != "memorabilia") %>%
  filter(set_type != "token") %>%
  filter(set_type != "funny") %>%
  filter(set_type != "planechase") %>%
  arrange(date_released,name,set_name) %>% 
  mutate(setcard_id = paste0(set_code,collector_number)) %>% 
  as_tibble() -> dta


######## Add split cards -------

split <- jsonlite::fromJSON("http://api.scryfall.com/cards/search?q=is%3Asplit")$data %>% 
  select(name) %>% 
  mutate(is_split = TRUE)

dta <- dta %>% 
  left_join(split, by = "name") %>% 
  select(mvid:is_reprint, is_split, everything()) %>% 
  mutate(is_split = ifelse(is.na(is_split),FALSE,is_split))



######## Write All Cards Table to SQLite DB ------
library(DBI)
library(RSQLite)

db <- dbConnect(SQLite(), "data/mtg_db.sqlite")

dbWriteTable(db, "cards_all", dta, overwrite=T)

dbReadTable(db, "cards_all") %>%
  head(10)

dbDisconnect(db)

######## Write All Cards Table to Postgres DB ------
library(DBI)
library(RPostgres)
con <- dbConnect(RPostgres::Postgres()
                 , host=host
                 , port='5432'
                 , dbname='mtgdb'
                 , user='postgres'
                 , password=ifelse(exists('pw'),
                                   pw,
                                   askpass::askpass())
                 )

dbWriteTable(con, "cards_all_tmp", dta, temporary = T)

dbExecute(con,
          "INSERT INTO cards_all (mvid,name,colors,color_id,set_name,set_code,set_type,date_released,mana_cost,cmc,type,oracle_text,power,toughness,has_foil,has_nonfoil,is_oversized,is_promo,is_reprint,is_split,variation,collector_number,digital,rarity,flavor_text,artist,keywords,loyalty,hand_modifier,price,price_foil,price_tix,standard,commander,pauper,modern,legacy,pioneer,historic,vintage,penny,data_created_date,scryfall_id,setcard_id)
          SELECT tmp.mvid,tmp.name,tmp.colors,tmp.color_id,tmp.set_name,tmp.set_code,tmp.set_type,tmp.date_released,tmp.mana_cost,tmp.cmc,tmp.type,tmp.oracle_text,tmp.power,tmp.toughness,tmp.has_foil,tmp.has_nonfoil,tmp.is_oversized,tmp.is_promo,tmp.is_reprint,tmp.is_split,tmp.variation,tmp.collector_number,tmp.digital,tmp.rarity,tmp.flavor_text,tmp.artist,tmp.keywords,tmp.loyalty,tmp.hand_modifier,tmp.price,tmp.price_foil,tmp.price_tix,tmp.standard,tmp.commander,tmp.pauper,tmp.modern,tmp.legacy,tmp.pioneer,tmp.historic,tmp.vintage,tmp.penny,tmp.data_created_date,tmp.scryfall_id,tmp.setcard_id
          FROM cards_all_tmp tmp
          LEFT JOIN cards_all act
          ON tmp.setcard_id = act.setcard_id
          WHERE act.setcard_id is null")

dta <- dbReadTable(con, "cards_all") %>% 
  as_tibble()


##### update price table with new daily data ----

if (data_updated_at==dbGetQuery(con, 
                                   "select date
                                   from price
                                   order by date desc
                                   limit 1") %>% 
    pull(date) %>% 
    lubridate::as_date()) {
  print("Prices already updated today")
}else{
  dbWriteTable(
    con, "price",
    dta %>%
      as_tibble() %>%
      select(price,
             price_foil,
             price_tix,
             setcard_id,
             card_id = id) %>%
      filter(!is.na(price) |
               !is.na(price_foil) | !is.na(price_tix), ) %>%
      mutate(date = data_updated_at) %>%
      select(date, price, price_foil, price_tix, setcard_id, card_id),
    overwrite = F,
    append = T
  )
}

##### Update Legalities Table -----
dta %>% 
  select(card_id = id,setcard_id, standard:penny) -> ltmp

## original table write
#dbWriteTable(con, "legalities" ,ltmp)

## write tmp table
dbWriteTable(con, 
             "legalities_tmp", 
             ltmp, 
             temporary = T,
             overwrite=T)

rm(ltmp)

dbExecute(con,
          "INSERT INTO legalities (card_id,setcard_id,standard,commander,pauper,modern,legacy)
          SELECT tmp.card_id,tmp.setcard_id,tmp.standard,tmp.commander,tmp.pauper,tmp.modern,tmp.legacy
          FROM legalities_tmp tmp
          LEFT JOIN legalities act
          ON tmp.setcard_id = act.setcard_id
          WHERE act.setcard_id is null")

dbExecute(con,
          "UPDATE legalities l
          SET 
            standard = t.standard, 
            commander = t.commander,
            pauper = t.pauper,
            modern = t.modern,
            legacy = t.legacy,
            pioneer = t.pioneer,
            historic = t.historic,
            vintage = t.vintage,
            penny = t.penny
          FROM legalities_tmp t
          WHERE l.setcard_id = t.setcard_id"
)

## DB Disconnect ----
dbDisconnect(con)




######################## Main Cards Table ---------------------
# drops extras

tbl <- dta %>% 
  filter(!is.na(mvid)) %>% 
  add_count(name, set_code) %>% 
  rename(multiple_prints = n) %>% 
  mutate(multiple_prints = ifelse(multiple_prints > 1, multiple_prints, NA)) %>% 
  group_by(name, set_code) %>% 
  mutate(main = ifelse(min(collector_number)==collector_number,1,NA),
         main = ifelse(str_detect(type, "Basic Land"), NA, main)) %>% 
  ungroup() %>% 
  rename(card_id = id)


######## Write All Cards Table to SQLite DB ------
library(DBI)
library(RSQLite)

db <- dbConnect(SQLite(), "data/mtg_db.sqlite")

dbWriteTable(db, "cards", tbl, overwrite=T)

dbReadTable(db, "cards") %>%
  head(10)

dbDisconnect(db); rm(db)

######## Write All Cards Table to Postgres DB ------
library(DBI)
library(RPostgres)
con <- dbConnect(RPostgres::Postgres()
                 , host=host
                 , port='5432'
                 , dbname='mtgdb'
                 , user='postgres'
                 , password=ifelse(exists('pw'),
                                   pw,
                                   askpass::askpass())
)




dbWriteTable(con, "cards_tmp", tbl, temporary = T)

dbExecute(con,
          "INSERT INTO cards (mvid,name,colors,color_id,set_name,set_code,set_type,date_released,mana_cost,cmc,type,oracle_text,power,toughness,has_foil,has_nonfoil,is_oversized,is_promo,is_reprint,is_split,variation,collector_number,digital,rarity,flavor_text,artist,keywords,loyalty,hand_modifier,price,price_foil,price_tix,standard,commander,pauper,modern,legacy,pioneer,historic,vintage,penny,data_created_date,scryfall_id,setcard_id, card_id, multiple_prints, main)
          SELECT tmp.mvid,tmp.name,tmp.colors,tmp.color_id,tmp.set_name,tmp.set_code,tmp.set_type,tmp.date_released,tmp.mana_cost,tmp.cmc,tmp.type,tmp.oracle_text,tmp.power,tmp.toughness,tmp.has_foil,tmp.has_nonfoil,tmp.is_oversized,tmp.is_promo,tmp.is_reprint,tmp.is_split,tmp.variation,tmp.collector_number,tmp.digital,tmp.rarity,tmp.flavor_text,tmp.artist,tmp.keywords,tmp.loyalty,tmp.hand_modifier,tmp.price,tmp.price_foil,tmp.price_tix,tmp.standard,tmp.commander,tmp.pauper,tmp.modern,tmp.legacy,tmp.pioneer,tmp.historic,tmp.vintage,tmp.penny,tmp.data_created_date,tmp.scryfall_id,tmp.setcard_id, tmp.card_id, tmp.multiple_prints, tmp.main
          FROM cards_tmp tmp
          LEFT JOIN cards_all act
          ON tmp.setcard_id = act.setcard_id
          WHERE act.setcard_id is null")

dbDisconnect(con); rm(con)
