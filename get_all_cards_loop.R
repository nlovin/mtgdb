library(httr)
library(tidyverse)

#mtg <- httr::GET("https://api.magicthegathering.io/v1/cards?name=cultivate")
#mtg_data <- httr::content(mtg)$cards


#mtg2 <- httr::GET("https://api.magicthegathering.io/v1/cards?page=515")
#mtg_data2 <- httr::content(mtg2)$cards

#test <- httr::content(mtg)


######## Get Bulk Card Data ------
res_bulk <- jsonlite::fromJSON("https://api.scryfall.com/bulk-data/default-cards")
# bulk_data <- read_rds("data/bulk.rds") %>% 
#   as_tibble()

######## Initial Get All Cards ------
res <- httr::GET("http://api.scryfall.com/cards/search?order=name&q=-type%3Abasic+legal%3Acommander+lang%3Aen&unique=prints")
resLS <- httr::content(res)$data

t <- Sys.time()

while (httr::content(res)$has_more == TRUE) {

  res <- httr::GET(httr::content(res)$next_page)
  resLS <- c(resLS, httr::content(res)$data)
  Sys.sleep(.1)
}

t2 <- Sys.time()


print(t2-t)

resLS <- read_rds("data/raw_cmd_list.rds")

tbl <- resLS %>%
  map(
    ~ tibble(
      name = .x$name,
      type = .x$type_line,
      rarity = .x$rarity,
      colors = .x[["colors"]] %>% unlist() %>% paste0(collapse = "|"),
      color_id = .x[["color_identity"]] %>% unlist() %>% paste0(collapse = "|"),
      set_code = .x$set,
      set_name = .x$set_name,
      set_type = .x$set_type,
      collector_number = .x$collector_number,
      mvid = .x$multiverse_id,
      artist = .x$artist,
      date_released = .x$released_at,
      mana_cost = .x$mana_cost,
      cmc = .x$cmc,
      is_reprint = .x$reprint,
      has_foil = .x$foil,
      has_nonfoil = .x$nonfoil,
      is_promo = .x$promo,
      is_oversized = .x$oversized,
      power = ifelse(is.null(.x$power),"",.x$power),
      toughness = ifelse(is.null(.x$toughness),"",.x$toughness),
      flavor_text = ifelse(is.null(.x$flavor_text),"",.x$flavor_text),
      oracle_text = ifelse(is.null(.x$oracle_text),"",.x$oracle_text),
      loyalty = ifelse(is.null(.x$loyalty),"",.x$loyalty),
      digital = .x$digital,
      variation = .x$variation,
      keywords = .x[["keywords"]] %>% unlist() %>% paste0(collapse = "|"),
      hand_modifier = NA,
      price = ifelse(is.null(.x[["prices"]][["usd"]]), "NA", .x[["prices"]][["usd"]]),
      price_foil = ifelse(is.null(.x[["prices"]][["usd_foil"]]), "NA", .x[["prices"]][["usd_foil"]]),
      price_tix = ifelse(is.null(.x[["prices"]][["tix"]]), "NA", .x[["prices"]][["tix"]]),
      standard = ifelse(is.null(.x[["legalities"]][["standard"]]),"",.x[["legalities"]][["standard"]]),
      commander = ifelse(is.null(.x[["legalities"]][["commander"]]),"",.x[["legalities"]][["commander"]]),
      pauper = ifelse(is.null(.x[["legalities"]][["pauper"]]),"",.x[["legalities"]][["pauper"]]),
      modern = ifelse(is.null(.x[["legalities"]][["modern"]]),"",.x[["legalities"]][["modern"]]),
      legacy = ifelse(is.null(.x[["legalities"]][["legacy"]]),"",.x[["legalities"]][["legacy"]]),
      pioneer = ifelse(is.null(.x[["legalities"]][["pioneer"]]),"",.x[["legalities"]][["pioneer"]]),
      historic = ifelse(is.null(.x[["legalities"]][["historic"]]),"",.x[["legalities"]][["historic"]]),
      vintage = ifelse(is.null(.x[["legalities"]][["vintage"]]),"",.x[["legalities"]][["vintage"]]),
      penny = ifelse(is.null(.x[["legalities"]][["penny"]]),"",.x[["legalities"]][["penny"]])
    )
  ) %>%
  reduce(bind_rows) %>%
  tidyr::unnest(cols = c(mvid)) %>%
  arrange(name) %>% 
  mutate(data_created_date = lubridate::today(),
         id = indx + row_number(),
         id_str = paste0(set_code,collector_number)) %>% 
  select(mvid,name,colors,color_id,set_name,set_code,set_type,date_released,mana_cost,cmc,type,oracle_text,power,toughness,has_foil,has_nonfoil,is_oversized,is_promo,is_reprint,variation,collector_number,digital,rarity,flavor_text,artist,keywords,loyalty,hand_modifier,price,price_foil,price_tix,standard,commander,pauper,modern,legacy,pioneer,historic,vintage,penny,data_created_date,id,id_str)


# tbl %>%
#   unnest(cols = c(mvid)) -> tbl





# initial writing of the DB
library(DBI)
library(RSQLite)

db <- dbConnect(SQLite(), "data/mtg_db.sqlite")

dbWriteTable(db, "edhcards", tbl)

dbReadTable(db, "edhcards") %>%
  head(10)

dbDisconnect(db)


####### SPLIT CARDS -------
res <- httr::GET("http://api.scryfall.com/cards/search?q=is%3Asplit&unique=prints")
resLS <- httr::content(res)$data

t <- Sys.time()

while (httr::content(res)$has_more == TRUE) {
  
  res <- httr::GET(httr::content(res)$next_page)
  resLS <- c(resLS, httr::content(res)$data)
  Sys.sleep(.1)
}

t2 <- Sys.time()


split <- resLS %>%
  map( ~ tibble(
    name = .x$name
  )) %>%
  reduce(bind_rows) %>% 
  select(name) %>% 
  distinct() %>% 
  pull(name)

tbl <- tbl %>% 
  mutate(is_split = ifelse(name %in% split,1,0))


db <- dbConnect(SQLite(), "data/mtg_db.sqlite")

dbWriteTable(db, "edhcards", tbl,overwrite=T)

dbReadTable(db, "edhcards") %>%
  head(10)

dbDisconnect(db)
