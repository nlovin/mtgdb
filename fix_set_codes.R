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
  select(Name = name) %>% 
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
                              set_code == "NE" ~ "NMS", # ACTUAL IS NEM NOT NMS
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
                              Name == "Sulfuric Vortex" ~ "DDK",
                              Name == "Sin Prodder" ~ "SOI",
                              Name == "Sin Prodder" ~ "SOI",
                              TRUE ~ as.character(set_code)) 
  ) %>% 
  mutate(set_code = case_when(Name == "Basalt Monolith" & set_code == "2ED" ~ "3ED",
                              Name == "Goblin Artisans" & set_code == "ATQ" ~ "CHR",
                              Name == "Ashnod's Altar" & set_code == "ATQ" ~ "CHR",
                              Name == "Goblin Rabblemaster" & set_code == "PM15" ~ "BAB",
                              Name == "Torbran, Thane of Red Fell" & set_code == "pELD" ~ "pPRE",
                              TRUE ~ as.character(set_code)),
         Name = ifelse(Name == "Jaya Ballard Emblem", "Emblem - Jaya Ballard",Name),
         Name = ifelse(Name == "Lim-Dûl's Vault", "Lim-Dul's Vault", Name),
         set_code = ifelse(set_code == "Spectral Grasp", "CN2", set_code),
         set_code = ifelse(Name == "Demolish", "WAR", set_code)) %>% 
  filter(set_code != "AMH1") %>% 
  left_join(split, by = "Name") %>% 
  mutate(Name = ifelse(is.na(is_split), 
                       str_remove_all(Name, " //.+"), 
                       Name)) %>%
  filter(!str_detect(Name, " Guildgate")) -> check


check %>% 
  filter(str_length(set_code) > 3)

write_csv(check,"coll.csv")          




