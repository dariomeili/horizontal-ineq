library(tidyverse)
library(haven)
library(magrittr)
library(sjmisc)
library(dineq)
# reading data
mozambique_1997 <- read_dta("data/mozambique/mozambique_men&women_1997.dta")
mozambique_2003 <- read_dta("data/mozambique/mozambique_men&women_2003.dta")
mozambique_2011 <- read_dta("data/mozambique/mozambique_men&women_2011.dta")
mozambique_2015 <- read_dta("data/mozambique/mozambique_men&women_2015.dta")

# merge data
moz <- bind_rows(list("1997" = mozambique_1997,
                      "2003" = mozambique_2003,
                      "2011" = mozambique_2011,
                      "2015" = mozambique_2015),
                 .id = "year")

# Generate sample weights
# moz %<>% 
#   mutate(wgt = v005/1000000)

# Variable of interest: completed years spent in school
moz %<>%  
  mutate(schooling = v133) %>% 
  filter(schooling < 30 & !is.na(schooling)) %>% # drop NA and limit obs to 30 years
  filter(v012 >= 15 & v012 <= 64) # set age limit to 15-64 according to ILO definition

# Religion
moz %<>% 
  mutate(reli = case_when(
    religion %in% c("catholic", 
                    "catholic (cat¢lica)") ~ "Roman catholic", 
    religion %in% c("protestant",
                    "protestant (protestante)",
                    "protestant / evangelic") ~ "Protestant",
    religion %in% c("anglican",
                    "evangelical/pentecostal",
                    "zion",
                    "zionist") ~ "Other christian",
    religion %in% c("islamic",
                    "islamic (mu‡ulman)",
                    "moslem") ~ "Muslim",
    religion %in% c("",
                    "animist",
                    "other",
                    "other (outra)") ~ "Other",
    religion %in% c("no religion",
                    "no religion (sem religiao)") ~ "No religion"),
    reli = as_factor(reli))

# Ethnicity
moz %<>% 
  mutate(eth = case_when(
    ethnicity %in% c("emakhuwa",
                     "emakua & simili") ~ "Emakhuwa",
    ethnicity %in% c("portugu?s",
                     "portugues") ~ "Portuguese",
    ethnicity %in% c("xichangana") ~ "Xichangana",
    ethnicity %in% c("cicewa") ~ "Chichewa",
    ethnicity %in% c("cisena",
                     "cisena & simili",
                     "cibalke"
    ) ~ "Cisena",
    ethnicity %in% c("elomue & emarenjo",
                     "elomwe") ~ "Elomwe",
    ethnicity %in% c("echuwabo") ~ "Echuwabo",
    ethnicity %in% c("cindau") ~ "Cindau",
    ethnicity %in% c("bitonga",
                     "xirhonga",
                     "xitsonga & simili",
                     "xitswa & simili") ~ "Xitsonga",
    ethnicity %in% c("chitewe",
                     "shona") ~ "Shona",
    ethnicity %in% c("",
                     "cichopi",
                     "cinyungwe",
                     "ciyao",
                     "coti",
                     "kimwane",
                     "missing",
                     "other",
                     "outra",
                     "shimakonde"
    ) ~ "Other/unknown"
  ), 
  eth = as_factor(eth))
# gender
moz %<>%
  mutate(gender = as_factor(gender))

# location (urban rural)
moz %<>% 
  mutate(location = as_factor(v025))

save(moz, file="moz.Rda")