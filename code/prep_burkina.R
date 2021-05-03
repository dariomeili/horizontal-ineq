# data preparation file for burkina
# filter observations with education above 30 years and column hv024 because R throws an error when trying to bind datasets
df$burkina_1993 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)
df$burkina_1999 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)
df$burkina_2003 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)
df$burkina_2010 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)

#extract data
burkina <- bind_rows(df$burkina_1993, df$burkina_1999, df$burkina_2003, df$burkina_2010)
burkina %<>%
  select(country_name, 
         year = survey_year,
         line_number = hvidx,
         cluster_id = hv001, 
         household_id = hv002,
         respondent_line = hv003, 
         wgt = hv005,
         sampling_unit = hv021, 
         stratification= hv025, 
         region = region_3, 
         residence = hv025, 
         wealth = hv271, 
         gender = gender, 
         age = v012, 
         religion = religion, 
         ethnicity = ethnicity,
         education = v133, 
         id
  )

burkina %<>% 
  mutate(country_name = "Burkina Faso")


burkina %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "Catholic" ),
                            "Protestant/other christian" = c("protestant", "protestant/anglican", "seventh-day adventist", "methodist", "protestant", "elcin", "Protestant"),
                            "Muslim" = c("islam", "moslem", "muslim", "moslem", "Muslim"), 
                            "Traditionalist" = c("traditional",  "traditional / animist", "traditionnal/animist", "Traditionnal/animist"),
                            "Other" = c("no religion", "other", "No religion", "other religion", "No religion", "Other", "without religion", "")
  ))

# harmonize ethnicity
burkina %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Mossi" = c("Mossi", "mossi"), 
                               "Fulani" = c("FulfuldÃ© / Peul", "fulfude (peul)", "fulfulde / peul", "fulfulde/peul", "fulfuldé / peul"),
                               "Gurma" = c("gourmantche", "gourmantch",  "gourmatche", "gourmantché", "gourmantchÃ©", "gourmatché"),
                               "Bobo" = c("Bobo", "bobo"),
                               "Gurunsi" = c("gourounsi", "Gourounsi", "gouroussi"),
                               "Senufo" = c("SÃ©noufo", "senoufo", "sÃ©noufo", "sÃ©noufo", "sénoufo", "senufo"),
                               "Dioula" = c("dioula", "Dioula"),
                               "Lobi" = c("lobi", "Lobi"), 
                               other_level = "Other")
         )
