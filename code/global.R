library(tidyverse)
library(haven)
library(sjmisc)
library(magrittr)
library(sjlabelled)
# load functions
source("code/functions.R")
# extract list of all files in dir
all_dfs <- list.files(path = "data", pattern = "*.dta", 
                      full.names = T)
# reading data
df <- all_dfs %>% 
  map(~read_plus(.)) 

# name the dataframes in list with country_year
names(df) = gsub("(data/|_ai_men&women|\\.dta)", "", all_dfs) 

# harmonize religion/ethnicity for each country
file.sources = list.files(path = "code", pattern="prep_", full.names = T)
sapply(file.sources, source, .GlobalEnv)

country_names <- gsub("(code/prep_|\\.R)", "", file.sources)

all <- bind_rows(benin, burkina, cameroon, chad, rep_congo, congo_dr, cote, ethiopia, gabon, gambia, ghana, guinea, kenya, liberia,
                 malawi, mali, mozambique, namibia, niger, nigeria, rwanda, senegal, sierra_leone, togo, uganda, zambia, zimbabwe)

all %<>%
  mutate(rel = fct_explicit_na(rel, na_level = "(Missing)"),
         rel = fct_collapse(rel, 
                          "Other" = "",
                          "Protestant/other christian" = "protestant (protestante)",
                          "Other" = "Traditionalist"),
         ethnic = fct_explicit_na(ethnic, na_level = "(Missing)"),
         ethnic = fct_collapse(ethnic, 
                               "Other" = ""))

save(all, file="data/prep_all.R")
