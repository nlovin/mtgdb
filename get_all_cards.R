library(httr)
library(tidyverse)

#mtg <- httr::GET("https://api.magicthegathering.io/v1/cards?name=cultivate")
#mtg_data <- httr::content(mtg)$cards


#mtg2 <- httr::GET("https://api.magicthegathering.io/v1/cards?page=515")
#mtg_data2 <- httr::content(mtg2)$cards

#test <- httr::content(mtg)


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

tbl <- resLS %>%
  map( ~ tibble(
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
    is_reprint = .x$reprint,
    price = ifelse(is.null(.x[["prices"]][["usd"]]), "0", .x[["prices"]][["usd"]]),
    price_foil = ifelse(is.null(.x[["prices"]][["usd_foil"]]), "0", .x[["prices"]][["usd_foil"]])
  )) %>%
  reduce(bind_rows) %>%
  unnest(cols = c(mvid)) %>%
  mutate(data_created_date = lubridate::today()
         # ,
         # white = ifelse(str_detect(color_id, "W"), "W", ""),
         # blue = ifelse(str_detect(color_id, "U"), "U", ""),
         # black = ifelse(str_detect(color_id, "B"), "B", ""),
         # red = ifelse(str_detect(color_id, "R"), "R", ""),
         # green = ifelse(str_detect(color_id, "G"), "G", ""),
         # color_id = paste0(white, blue, black, red, green),
         # white = ifelse(str_detect(color_id, "W"), 1, 0),
         # blue = ifelse(str_detect(color_id, "U"), 1, 0),
         # black = ifelse(str_detect(color_id, "B"), 1, 0),
         # red = ifelse(str_detect(color_id, "R"), 1, 0),
         # green = ifelse(str_detect(color_id, "G"), 1, 0)
  )


# tbl %>%
#   unnest(cols = c(mvid)) -> tbl


library(DBI)
library(RSQLite)


# initial writing of the DB
db <- dbConnect(SQLite(), "mtg_db.sqlite")

dbWriteTable(db, "edhcards", tbl)

dbReadTable(db, "edhcards") %>%
  head(10)

dbDisconnect(db)

write_rds(tbl, "edhcards.rds")



