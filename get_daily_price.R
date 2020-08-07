# update daily price data from scryfall

library(DBI)
library(RPostgres)
con <- dbConnect(RPostgres::Postgres()
                 , host='192.168.1.84'
                 , port='5432'
                 , dbname='mtgdb'
                 , user='postgres'
                 , password=ifelse(exists('pw'),
                                   pw,
                                   askpass::askpass())
)

##### initial price table write ----
# dbWriteTable(
#   con, "price",
# dta %>%
#   as_tibble() %>%
#   select(price,
#          price_foil,
#          price_tix,
#          setcard_id,
#          card_id = id) %>%
#   filter(!is.na(price) |
#            !is.na(price_foil) | !is.na(price_tix), ) %>%
#   mutate(date = lubridate::today()) %>%
#   select(date, price, price_foil, price_tix, setcard_id, card_id),
# overwrite = T
# )

##### update price table with new daily data ----
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
  mutate(date = lubridate::today()) %>%
  select(date, price, price_foil, price_tix, setcard_id, card_id),
overwrite = F,
append = T
)