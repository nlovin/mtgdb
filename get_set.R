get_set <- function(setcode,
                    output = NULL,
                    append = FALSE) {
  request <-
    glue::glue("https://api.scryfall.com/sets/{setcode}?order=name")
  
  res <- httr::GET(httr::content(
    httr::GET(request)
  )$search_uri)
  
  resLS <-
    httr::content(httr::GET(httr::content(
      httr::GET(request)
    )$search_uri))$data
  
  while (httr::content(res)$has_more == TRUE) {
    Sys.sleep(.1)
    res <- httr::GET(httr::content(res)$next_page)
    resLS <- c(resLS, httr::content(res)$data)
  }
  
  if (append == TRUE) {
    library(dplyr)
    library(purrr)
    indx <- DBI::dbReadTable(DBI::dbConnect(RPostgres::Postgres()
                           , host='localhost'
                           , port='5432'
                           , dbname='mtgdb'
                           , user='postgres'
                           , password=ifelse(exists('pw'),
                                             pw,
                                             askpass::askpass())),
                           "cards"
    ) %>% 
      pull(id) %>% 
      max()
    
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
    
    print("Connecting to DB")
    
    dbWriteTable(
      DBI::dbConnect(RSQLite::SQLite(), "data/mtg_db.sqlite"),
      "edhcards",
      tbl,
      overwrite = F,
      append = T
    )
    
    print("New set appended to DB")
    
  } else if (missing(output)) {
    return(resLS)
    
  } else if (output %in% c("table", "list")) {
    if (output == "table") {
      library(dplyr)
      library(purrr)
      indx <- DBI::dbReadTable(DBI::dbConnect(RPostgres::Postgres()
                                              , host='localhost'
                                              , port='5432'
                                              , dbname='mtgdb'
                                              , user='postgres'
                                              , password=ifelse(exists('pw'),
                                                                pw,
                                                                askpass::askpass())),
                               "cards"
      ) %>% 
        pull(id) %>% 
        max()
      
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
               id = indx + row_number()) %>% 
        select(mvid,name,colors,color_id,set_name,set_code,set_type,date_released,mana_cost,cmc,type,oracle_text,power,toughness,has_foil,has_nonfoil,is_oversized,is_promo,is_reprint,variation,collector_number,digital,rarity,flavor_text,artist,keywords,loyalty,hand_modifier,price,price_foil,price_tix,standard,commander,pauper,modern,legacy,pioneer,historic,vintage,penny,data_created_date,id,id_str)
      return(tbl)
    }
    else{
      return(resLS)
    }
    
  } else {
    warning("Error: Provided incorrect output type. Returning list format.")
    return(resLS)
  }
  
  
  
}

# gets set and returns table
# get_set("iko", output = "table")

# gets set and appends it to DB
# get_set("iko", append = T)
