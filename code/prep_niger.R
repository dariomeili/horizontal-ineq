# data preparation file for niger
#extract data
niger <- bind_rows(df$niger_1992, df$niger_1998)

niger %<>%
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

niger %<>% 
  mutate(country_name = "Niger")

# harmonize religion
niger %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("christrian", "christian"),
                            "Muslim" = c("muslem", "muslim"),
                            "Other" = c("no religion" , "other", "", "animist")
  ))

# harmonize ethnicity
niger %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "djerma" = c("djerma", "djerma/songhai"), 
                               "peul" = c("peul", "peulh"),
                               "touareg" = c( "touareg", "touareg bella"), 
                               "haoussa" = c("haoussa"), 
                               "kanouri" = c("kanouri"), 
                               "other" = c("other non-niger" , "other", "autre", "mossi", "", "gourmantch", "gourmanthe", "toubou", "arab", "arabe")
  ))


