library(tidyverse)

col <- read_csv(file.choose())

needs_fix <- col %>% 
  filter(str_length(set_code) < 3)

needs_fix_setsonly <- col %>% 
  filter(str_length(set_code) < 3) %>% 
  select(set_code) %>% 
  distinct()

# fix_sets_echo <- . %>% 
#   mutate(set_code = case_when(set_code == "2U" ~ "2ED",
#                               set_code == "3E" ~ "3ED",
#                               set_code == "4E" ~ "4ED",
#                               set_code == "5E" ~ "5ED",
#                               set_code == "6E" ~ "6ED",
#                               set_code == "7E" ~ "7ED",
#                               set_code == "AL" ~ "ALL",
#                               set_code == "AP" ~ "APC",
#                               set_code == "AQ" ~ "ATQ",
#                               set_code == "CG" ~ "UDS",
#                               set_code == "DK" ~ "DRK",
#                               set_code == "EX" ~ "EXO",
#                               set_code == "FE" ~ "FEM",
#                               set_code == "GU" ~ "ULG",
#                               set_code == "HM" ~ "HML",
#                               set_code == "IA" ~ "ICE",
#                               set_code == "IN" ~ "INV",
#                               set_code == "MI" ~ "MIR",
#                               set_code == "MM" ~ "MMQ",
#                               set_code == "NE" ~ "NMS", # ACTUAL IS NEM NOT NMS
#                               set_code == "OD" ~ "ODY",
#                               set_code == "PR" ~ "PCY",
#                               set_code == "PS" ~ "PLS",
#                               set_code == "ST" ~ "STH",
#                               set_code == "TE" ~ "TMP",
#                               set_code == "UZ" ~ "USG",
#                               set_code == "VI" ~ "VIS",
#                               set_code == "WL" ~ "WTH",
#                               set_code == "E02" ~ "EO2",
#                               set_code == "PRES" ~ "PD2",
#                               set_code == "PSLD" ~ "SLD",
#                               set_code == "DD3_EVG" ~ "EVG",
#                               Name == "Sulfuric Vortex" ~ "DDK",
#                               Name == "Sin Prodder" ~ "SOI",
#                               TRUE ~ as.character(set_code))
#          )

split <- jsonlite::fromJSON("http://api.scryfall.com/cards/search?q=is%3Asplit")$data %>% 
  select(card_name = name) %>% 
  mutate(is_split = TRUE)

col %>% 
  mutate(set_code = case_when(set_code == "2U" ~ "2ED",
                              set_code == "3E" ~ "3ED",
                              set_code == "4E" ~ "4ED",
                              set_code == "5E" ~ "5ED",
                              set_code == "6E" ~ "6ED",
                              set_code == "7E" ~ "7ED",
                              set_code == "AL" ~ "ALL",
                              set_code == "AP" ~ "APC",
                              set_code == "AQ" ~ "ATQ",
                              set_code == "CG" ~ "UDS",
                              set_code == "DK" ~ "DRK",
                              set_code == "EX" ~ "EXO",
                              set_code == "FE" ~ "FEM",
                              set_code == "GU" ~ "ULG",
                              set_code == "HM" ~ "HML",
                              set_code == "IA" ~ "ICE",
                              set_code == "IN" ~ "INV",
                              set_code == "MI" ~ "MIR",
                              set_code == "MM" ~ "MMQ",
                              set_code == "NE" ~ "NEM", # ACTUAL IS NEM NOT NMS, but NMS is what echomtg calls it
                              set_code == "OD" ~ "ODY",
                              set_code == "PR" ~ "PCY",
                              set_code == "PS" ~ "PLS",
                              set_code == "ST" ~ "STH",
                              set_code == "TE" ~ "TMP",
                              set_code == "UZ" ~ "USG",
                              set_code == "VI" ~ "VIS",
                              set_code == "WL" ~ "WTH",
                              set_code == "E02" ~ "EO2",
                              set_code == "PRES" ~ "PD2",
                              set_code == "PSLD" ~ "SLD",
                              set_code == "DD3_EVG" ~ "EVG",
                              set_code == "DDO" ~ "EVK",
                              set_code == "PDOM" ~ "pPRE",
                              set_code == "TMED" ~ "MED2",
                              set_code == "FNM" ~ "pFNM",
                              set_code == "PGRN" ~ "pPRE",
                              set_code == "PPC1" ~ "pPRE",
                              set_code == "PTHS" ~ "pREL2",
                              set_code == "PELD" ~ "pELD",
                              card_name == "Sulfuric Vortex" ~ "DDK",
                              card_name == "Sin Prodder" ~ "SOI",
                              card_name == "Sin Prodder" ~ "SOI",
                              TRUE ~ as.character(set_code)) 
  ) %>% 
  mutate(set_code = case_when(card_name == "Basalt Monolith" & set_code == "2ED" ~ "3ED",
                              card_name == "Goblin Artisans" & set_code == "ATQ" ~ "CHR",
                              card_name == "Ashnod's Altar" & set_code == "ATQ" ~ "CHR",
                              card_name == "Goblin Rabblemaster" & set_code == "PM15" ~ "BAB",
                              card_name == "Torbran, Thane of Red Fell" & set_code == "pELD" ~ "pPRE",
                              TRUE ~ as.character(set_code)),
         card_name = ifelse(card_name == "Jaya Ballard Emblem", "Emblem - Jaya Ballard",card_name),
         card_name = ifelse(card_name == "Lim-Dûl's Vault", "Lim-Dul's Vault", card_name),
         set_code = ifelse(set_code == "Spectral Grasp", "CN2", set_code),
         set_code = ifelse(card_name == "Demolish", "WAR", set_code)) %>% 
  filter(set_code != "AMH1") %>% 
  left_join(split, by = "card_name") %>% 
  mutate(card_name = ifelse(is.na(is_split), 
                       str_remove_all(card_name, " //.+"), 
                       card_name)) %>%
  filter(!str_detect(card_name, " Guildgate")) -> check


check %>% 
  filter(str_length(set_code) > 3)

write_csv(check,"coll2.csv")        




