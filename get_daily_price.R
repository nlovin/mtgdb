# update daily price data from scryfall

### Data updated at
data_updated_at <- lubridate::with_tz(lubridate::as_datetime(res_bulk$updated_at),
                                      tzone = "America/New_York")

library(DBI)
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
#   mutate(date = data_updated_at) %>%
#   select(date, price, price_foil, price_tix, setcard_id, card_id),
# overwrite = T
# )

##### update price table with new daily data ----

if (lubridate::today()==dbGetQuery(con, 
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
      mutate(date = lubridate::today()) %>%
      select(date, price, price_foil, price_tix, setcard_id, card_id),
    overwrite = F,
    append = T
  )
}



