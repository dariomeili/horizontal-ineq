library(tidyverse)
library(haven)
library(magrittr)
library(sjmisc)
library(dineq)
# reading data
load("temp.Rda")
# # Generate sample weights
# gha %<>%
#   mutate(wgt = v005/10000000)


df %>% 
  mutate(reli = ifelse(country == "ghana", 
                       case_when(ethnicity %in% c("akan",
                                                  "other akan",
                                                  "akwapim",
                                                  "asante",
                                                  "fante") ~ "Akan",
                                 ethnicity %in% c("ga/adangbe", "ga/dangme") ~ "Ga/Dangme",
                                 ethnicity %in% c("ewe") ~ "Ewe",
                                 ethnicity %in% c("guan") ~ "Guan",
                                 ethnicity %in% c("mole-dagbani") ~ "Mole-Dagbani", 
                                 ethnicity %in% c("grusi", "grussi") ~ "Grusi",
                                 ethnicity %in% c("gruma", "gurma") ~ "Gurma",
                                 ethnicity %in% c("mande") ~ "Mande",
                                 ethnicity %in% c("", 
                                                  "hausa",
                                                  "other",
                                                  "dagarti",
                                                  "mande") ~ "Other",
                                 TRUE ~ NA_character_
                       ), 
                       reli)) %>% 
  frq(reli)
         
         
case_if <- function(.data, country, ... ) {
  if (country == .data[[country]]) {
    case_when(...)
  } else {
    
  }
}

# Ethnicity
df %<>% 
  mutate(eth = case_when(
    ethnicity %in% c("akan",
                     "other akan",
                     "akwapim",
                     "asante",
                     "fante") ~ "Akan",
    ethnicity %in% c("ga/adangbe", "ga/dangme") ~ "Ga/Dangme",
    ethnicity %in% c("ewe") ~ "Ewe",
    ethnicity %in% c("guan") ~ "Guan",
    ethnicity %in% c("mole-dagbani") ~ "Mole-Dagbani", 
    ethnicity %in% c("grusi", "grussi") ~ "Grusi",
    ethnicity %in% c("gruma", "gurma") ~ "Gurma",
    ethnicity %in% c("mande") ~ "Mande",
    ethnicity %in% c("", 
                     "hausa",
                     "other",
                     "dagarti",
                     "mande") ~ "Other",
    TRUE ~ NA_character_
  ))
# religion
df %<>% 
  mutate(reli = case_when(
    religion %in% c("catholic",
                    "roman catholic") ~ "Catholic", 
    religion %in% c("pentecostal/charismatic") ~ "Pentecostal/Charismatic",
    religion %in% c("anglican", 
                    "methodist",
                    "presbyterian") ~ "Anglican/Methodist/Presbyterian",
    religion %in% c("other christian") ~ "Other christian",
    religion %in% c("islam",
                    "moslem") ~ "Muslim",
    religion %in% c("traditional",
                    "traditional/spiritualist",
                    "spiritualist") ~ "Traditional/Spiritualist",
    religion %in% c("",
                    "other") ~ "Other",
    religion %in% c("no religion",
                    "no religion + other") ~ "No religion",
    TRUE ~ NA_character_
    ))

# gender
gha %<>%
  mutate(gender = as_factor(gender))

# location (urban rural)
gha %<>% 
  mutate(location = as_factor(v025))

save(gha, file="gha.Rda")