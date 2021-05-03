# data preparation file for guinea
#extract data
guinea <- bind_rows(df$guinea_1999, df$guinea_2005, df$guinea_2012)

guinea %<>%
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

guinea %<>% 
  mutate(country_name = "Guinea")

# harmonize religion
guinea %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("Christian", "christian"), 
                            "Muslim" = c("Muslim", "muslim"),
                            "Traditionalist" = c("Animist", "animist"),
                            "Other" = c("No religion" , "Other", "no religion" , "other"),
                            
  ))

# harmonize ethnicity
guinea %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                                "Guerzé"  = c("Guerzé", "guerze", "guerzé"), 
                                "Kissi"  = c("Kissi", "kissi"),
                                "Malinké"  = c("Malinké", "malinke", "malinké"), 
                                "Soussou"  = c("Soussou", "sousou",  "soussou"),
                                "Peulh"  = c("Peulh", "peulh"), 
                                "Toma"  = c("toma", "Toma"),
                                "Other"  = c("Other", "other")
  ))


