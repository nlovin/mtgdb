get_set <- function(setcode, output=NULL){
  
  request <- glue::glue("https://api.scryfall.com/sets/{setcode}?order=name")
  
  resLS <- httr::content(httr::GET(httr::content(httr::GET("https://api.scryfall.com/sets/iko?order=name"))$search_uri))$data
  
  
  
  if (missing(output)) {
    return(resLS)
  } else if (output %in% c("table", "list")) {
    if (output == "table") {
      library(dplyr)
      library(purrr)
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
        tidyr::unnest(cols = c(mvid)) %>%
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
      return(tbl)
    }
    else{
      return(resLS)
    }
    
  } else {
    warning("Error: Provided incorrect output type. Returning list format.")
    return(resLS)
  }

  
  # if (append == TRUE){
  #   db <- DBI::dbConnect(RSQLite::SQLite(), "data/mtg_db.sqlite")
  #   
  #   dbWriteTable(db, "edhcards_test", tbl,overwrite=F, append=T)
  #   
  #   dbDisconnect(db)
  # }
  
  
  
}


get_set("iko", output = "table", append = TRUE)
