######## Write All Cards Table to Postgres DB ------
library(tidyverse)
library(janitor)
library(DBI)
library(RPostgres)
library(lubridate)
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
  as_tibble() %>% 
  filter()

rares <- tbl %>% 
  filter(rarity == "rare" | rarity == "mythic")


dbGetQuery(
  con, 
  "select mvid, name, cmc, set_name, set_code, type, c.date_released, c.set_type, c.setcard_id, c.card_id, ph.date, ph.price, c.is_reprint, c.multiple_prints
  from cards c 
  join price_history ph on 
  c.setcard_id = ph.setcard_id 
  where rarity = 'rare' or rarity = 'mythic'"
) %>% 
  as_tibble() -> ph

ph %>% 
  mutate(date = as_date(date),
         date_released = as_date(date_released)) -> ph

ph %>% 
  filter(date_released > as_date("2010-01-01")) -> phm


phm %>% 
  as_tibble() %>% 
  mutate(date = as_date(date),
         date_released = as_date(date_released)) -> phm

phm %>% 
  mutate(days_since_released = date - date_released) -> phm

# remove pre-launch prices
phs <- phm %>% 
  mutate(days_since_released = as.numeric(days_since_released)) %>% 
  filter(days_since_released > 0)

# earliest date in the data for price is 11.02.2010 so splitting off from sets before this
pre_nov_2010 <- phs %>% filter(date_released < as_date("2010-11-01"))
pn <- phs %>% filter(date_released > as_date("2010-11-01"))

# checking min days since set launch
pre_nov_2010 %>% group_by(set_code) %>% summarise(c = min(days_since_released)) %>% filter(c > 1)
pn %>% group_by(set_code) %>% summarise(c = min(days_since_released)) %>% filter(c > 1)

# n rares in set
pn %>% 
  select(set_name, name) %>% 
  distinct() %>% 
  group_by(set_name) %>% 
  summarize(count = n())

# trim cards look at the third week since release
dta <- pn %>% filter(days_since_released > 21) %>% filter(days_since_released < 29)

# average rare price by set
dta %>% 
  filter(set_type != "masters") %>% 
  group_by(set_name) %>% 
  mutate(avg_price = mean(price)) %>% 
  select(set_name, set_code, avg_price, set_type) %>% 
  distinct() %>% 
  arrange(avg_price) %>% 
  ggplot() +
  geom_bar(aes(y = reorder(as.factor(set_name),avg_price), x = avg_price, fill = factor(set_type)), stat = 'identity')


# counting bulk at launch
dta %>% 
  filter(set_type != "masters") %>% 
  group_by(set_name, setcard_id) %>% 
  mutate(avg_price = mean(price)) %>% 
  ungroup() %>% 
  select(name, set_name, set_code, avg_price, set_type) %>% 
  distinct() %>% 
  mutate(bulk = ifelse(avg_price < 2, 1, 0)) -> bulk

bulk %>% 
  group_by(set_name) %>% 
  summarise(n_bulk = sum(bulk)) %>% 
  ggplot() +
  geom_bar(aes(y = reorder(as.factor(set_name),n_bulk), x = n_bulk), stat = 'identity')



# counting bulk after a year
pn %>% 
  filter(days_since_released < 368) %>% 
  filter(days_since_released > 364) %>% 
  filter(set_type != "masters") %>% 
  group_by(set_name, setcard_id) %>% 
  mutate(avg_price = mean(price)) %>% 
  ungroup() %>% 
  select(name, set_name, set_code, avg_price, set_type) %>% 
  distinct() %>% 
  mutate(bulk = ifelse(avg_price < 2, 1, 0)) -> bulk_year

bulk_year %>% 
  group_by(set_name) %>% 
  summarise(n_bulk = sum(bulk)) %>% 
  ggplot() +
  geom_bar(aes(y = reorder(as.factor(set_name),n_bulk), x = n_bulk), stat = 'identity')
