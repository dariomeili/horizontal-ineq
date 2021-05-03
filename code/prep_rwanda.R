# data preparation file for rwanda
#extract data
rwanda <- bind_rows(df$rwanda_1992, df$rwanda_2005, df$rwanda_2008, df$rwanda_2010, df$rwanda_2015)

rwanda %<>%
  filter(v133 <= 30) %>% # filter out observations with more than 30 years of education
  select(country_name, 
         year = survey_year,
         line_number = hvidx,
         cluster_id = hv001, 
         household_id = hv002,
         respondent_line = hv003, 
         wgt = hv005,
         sampling_unit = hv021, 
         stratification= hv025, 
         residence = hv025, 
         gender = gender, 
         age = v012, 
         religion = religion, 
         ethnicity = ethnicity,
         education = v133, 
         id
  )

rwanda %<>% 
  mutate(country_name = "Rwanda")

# harmonize religion
rwanda %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "catholique"), 
                            "Protestant/other christian" = c("7th day adventist", "adventist", "adventiste", "eglise du 7e jour", "jehovah witness", "protestant"),
                            "Muslim" = c("muslim", "muslman"),
                            "Other" = c("no religion", "aucune", "other", "", "autre", "none", "religion traditionelle", "traditional", "traditional religion")
  ))

# harmonize ethnicity
rwanda %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "hutu" = "hutu",
                               "tutsi" = "tutsi",
                               "other" = c("twa", "other", "")
  ))


