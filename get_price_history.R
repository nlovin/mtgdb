#get price history

#### Function ----

get_set_prices <- function(set){
  
  library(tidyverse)
  library(httr)
  library(rvest)
  
  ## Set name
  ifelse(set == "Magic 2014", "Magic 2014 Core Set", set) %>%
    ifelse(. == "Magic 2015", "Magic 2015 Core Set", .) %>%
    ifelse(. == "Time Spiral Timeshifted", "Timeshifted", .) %>%
    str_replace_all(" ", "+") %>%
    str_remove_all(":") %>%
    str_remove_all("'") -> set_url
    
  
  
  ### Get set cards
  library(DBI)
  library(RSQLite)
  library(RPostgres)
  pw <- askpass::askpass()
  con <- dbConnect(RPostgres::Postgres(),
                   host='localhost',
                   port='5432',
                   dbname='mtgdb',
                   user='postgres',
                   password=pw)
  
  set_urls <- dbReadTable(con,"cards_all") %>%
    as_tibble() %>% 
    filter(set_name == set) %>% 
    filter(!str_detect(type, "Basic Land")) %>% 
    filter(ifelse(set_type == "masters", !is.na(id), main == 1)) %>% 
    mutate(name = str_remove_all(name, "'"),
           name = str_remove_all(name, ","),
           name = ifelse(is_split==T, str_replace(name, " // ","+"),name),
           name = str_remove_all(name, " //.+"),
           name = str_replace_all(name, " ", "+"),
           name = paste0("https://www.mtggoldfish.com/price/",
                         set_url,
                         "/",
                         name,
                         "#paper")) %>% 
    distinct() %>% 
    pull(name)
  
  
  set_code <- dbReadTable(con,"cards_all") %>%
    as_tibble() %>% 
    filter(set_name == set) %>% 
    filter(!str_detect(type, "Basic Land")) %>% 
    filter(ifelse(set_type == "masters", !is.na(id), main == 1)) %>% 
    select(set_code) %>% 
    distinct() %>% 
    pull(set_code)
  
  set_card_names <- dbReadTable(con,"cards_all") %>%
    as_tibble() %>% 
    filter(set_name == set) %>% 
    filter(!str_detect(type, "Basic Land")) %>% 
    filter(ifelse(set_type == "masters", !is.na(id), main == 1)) %>% 
    distinct() %>% 
    pull(name)
  
  set_card_ids <- dbReadTable(con,"cards_all") %>%
    as_tibble() %>% 
    filter(set_name == set) %>% 
    filter(!str_detect(type, "Basic Land")) %>% 
    filter(ifelse(set_type == "masters", !is.na(id), main == 1)) %>% 
    distinct() %>% 
    pull(setcard_id)
  
  loop_tbl <- bind_cols(set_urls,set_card_names,set_card_ids) %>% 
    rename(set_urls = 1, set_card_names = 2, setcard_id=3) %>% 
    filter(!str_detect(set_card_names, "Guildgate")) %>% 
    left_join(., dbReadTable(con,"cards_all") %>%
                as_tibble() %>% 
                select(id, setcard_id), by = "setcard_id")

  dbDisconnect(con)
  
  hist <- tibble()
  
  for (i in 1:length(loop_tbl$set_urls)) {

    
    indx <- match(loop_tbl$set_card_names[i], loop_tbl$set_card_names)
    
    tryCatch(
      
      expr = {
    
    s <- str_squish(content(GET(loop_tbl$set_urls[i]), as = 'text'))
    
    str_extract(s, 'price-sources-paper"\\)\\.toggle\\(true\\).+containing div') %>% 
      str_remove_all('\\+= \\"\\\\n') %>% 
      str_remove_all('\\"') %>% 
      str_remove_all("g = new Dygraph.+") %>% 
      str_remove_all(" d ") %>% 
      str_split(";") %>% 
      unlist() -> tmp
    
    nme <- tmp[2] %>% 
      str_remove(" var= Date,")
    
    if (str_detect(nme,"&#39")) {
      nme <- paste(tmp[c(2,3)], collapse = "") %>% 
        str_replace("&#39", "'") %>% 
        str_remove(" var= Date,")
      
      tmp[-1:-3] %>% 
        str_remove_all(" d ") %>% 
        str_split(",", simplify = T) %>% 
        as_tibble(.name_repair = "minimal") %>% 
        rename(date = 1, price = 2) %>% 
        filter(date != "") %>% 
        mutate(price = as.numeric(price),
               name = loop_tbl$set_card_names[i],
               set_code = set_code,
               set_name = set,
               setcard_id = loop_tbl$setcard_id[i]) %>% 
        filter(!is.na(price)) %>% 
        mutate(date = lubridate::as_date(date)) -> tmp
    } else{
      tmp[-1:-2] %>% 
        str_remove_all(" d ") %>% 
        str_split(",", simplify = T) %>% 
        as_tibble(.name_repair = "minimal") %>% 
        rename(date = 1, price = 2) %>% 
        filter(date != "") %>% 
        mutate(price = as.numeric(price),
               name = loop_tbl$set_card_names[i],
               set_code = set_code,
               set_name = set,
               setcard_id = loop_tbl$setcard_id[i]) %>% 
        filter(!is.na(price)) %>% 
        mutate(date = lubridate::as_date(date)) -> tmp
    }
    
    hist <- bind_rows(hist, tmp)
    
    print(paste0("Finished with ", loop_tbl$set_card_names[i]))
    message("Iteration ", indx, " successful.")
    t <- runif(1,2.5,5)
    print(paste0("Sleeping for ", t, " seconds"))
    Sys.sleep(t)
      },
    error = function(e){
      message("* Caught an error on itertion ", indx, ". URL: ", loop_tbl$set_urls[i])
      print(e)
      Sys.sleep(1)
    }
    
    )
    
  }
  
  print("Finished. Writing to DB now.")
  
  con <- dbConnect(RPostgres::Postgres(),
                   host='localhost',
                   port='5432',
                   dbname='mtgdb',
                   user='postgres',
                   password=pw)
  # add name
  hist <- hist %>% 
    select(date, 
           price, 
           name, 
           set_code, 
           set_name, 
           setcard_id)
  
  # Write tmp file for inspection
  write_rds(hist, paste0("data/tmp/tmp_price_",lubridate::today() %>% str_replace_all("-","_"),".rds"))
  
  # Return top of file
  hist %>% head(15)
  
  # write to DB
  dbWriteTable(con, 
               "price_history", 
               hist %>% 
                 select(date, 
                        price, 
                        setcard_id), 
               overwrite=F, 
               append=T)

  
  dbDisconnect(con)
  
}
