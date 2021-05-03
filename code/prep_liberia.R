# data preparation file for liberia
#extract data
liberia <- bind_rows(df$liberia_2007, df$liberia_2013)

liberia %<>%
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

liberia %<>% 
  mutate(country_name = "Liberia")

# harmonize religion
liberia %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("christian"), 
                            "Muslim" = c("muslim"),
                            "Other" = c("no religion" , "other", "traditional religion", "")
  ))

# harmonize ethnicity
liberia %<>% 
  mutate(ethnic = as_factor(ethnicity)
  )


