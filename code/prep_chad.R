# data preparation file for chad
# filter observations with education above 30 years and column hv024 because R throws an error when trying to bind datasets
df$chad_1997 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)


#extract data
chad <- bind_rows(df$chad_1997,
                     df$chad_2004) %>% 
  filter(v133 <= 30)

chad %<>%
  select(country_name = hv000, 
         year = survey_year,
         line_number = hvidx,
         cluster_id = hv001, 
         household_id = hv002,
         respondent_line = hv003, 
         wgt = hv005,
         sampling_unit = hv021, 
         stratification= hv025, 
         residence = hv025, 
         wealth = hv271, 
         gender = gender, 
         age = v012, 
         religion = religion, 
         ethnicity = v131,
         education = v133, 
         id
  )

chad %<>% 
  mutate(country_name = "Chad")

# harmonize religion
chad %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"),
                            "Protestant/other christian" = c("protestant"),
                            "Muslim" = c("muslim", "muslim/islam"), 
                            "Traditionalist" = c("animist"),
                            "Other" = c("no religion", "other")
  ))

# harmonize ethnicity
chad %<>% 
  mutate(ethnic = fct_collapse(as_factor(ethnicity), 
                               "Arab" = c("2"), 
                               "Baguirmien" = c("4"), 
                               "Fitri-Batha" = c("6"), 
                               "Gorane" = c("1"), 
                               "Hadjarai" = c("7"), 
                               "Kanem-Bornou" = c("5"), 
                               "Peul" = c("11"), 
                               "Sara" = c("9"), 
                               "Ouaddai" = c("3"),
                               "Kebbi" = c("12"),
                               "Tandjile" = c("10"), 
                               "Lac Iro" = c("8"),  
                               "Other" = c("13" , "14" , "99")),
         ethnicity = as_character(as_factor(ethnicity))
  )

