# data preparation file for gambia
#extract data
gambia <- bind_rows(df$Gambia_2013)

gambia %<>%
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

gambia %<>% 
  mutate(country_name = "The Gambia")

# harmonize religion
gambia %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("christianity"),
                            "Muslim" = c("islam"), 
                            "Other" = c("no religion", "")
  ))

# harmonize ethnicity
gambia %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "fula/tukulur/lorobo" = "fula/tukulur/lorobo",
                               "jola/karoninka" = "jola/karoninka",
                               "manjago" = "manjago",
                               "serahuleh" = "serahuleh",
                               "serere" = "serere",
                               "wollof" = "wollof",
                               "Other" = c("non-gambian", "other", "", "bambara", "creole/aku marabout"),
  ))


