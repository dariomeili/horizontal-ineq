# data preparation file for zimbabwe
#extract data
zimbabwe <- bind_rows(df$zimbabwe_1994, df$zimbabwe_1999, df$zimbabwe_2006, df$zimbabwe_2011, df$zimbabwe_2015)

zimbabwe %<>%
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
         ethnicity = v131,
         education = v133, 
         id
  )

zimbabwe %<>% 
  mutate(country_name = "Zimbabwe")

# harmonize religion
zimbabwe %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("christian"),
                            "Catholic" = c("roman catholic"), 
                            "Protestant/other christian" = c("apostolic sect", "other christian", "pentecostal", "protestant"),
                            "Muslim" = c("muslim"),
                            "Other" = c("", "none", "other", "spiritual", "traditional")
  ))

# harmonize ethnicity
zimbabwe %<>% 
  mutate(ethnicity = as_character(ethnicity), 
         ethnic = ethnicity)
  


