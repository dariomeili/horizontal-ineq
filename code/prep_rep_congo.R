# data preparation file for rep_congo
#extract data
rep_congo <- bind_rows(df$congo_2005, df$congo_2012)

rep_congo %<>%
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

rep_congo %<>% 
  mutate(country_name = "Congo Republic")

# harmonize religion
rep_congo %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "catholique"),
                            "Protestant/other christian" = c("adventist/jehova", "adventiste/jehova", "arm‚e du salut", "eglise de réveil",
                                                             "kibanguist", "kimbanguiste", "protestant", "salvation army", 
                                                             "zephirin/matsouaniste/ngunza", "zephirrin/matsouanist/ngunza"),
                            "Muslim" = c("muslim", "musulman"),
                            "Other" = c("" , "animist", "animiste", "no religion", "none", "other")
  ))

# harmonize ethnicity
rep_congo %<>% 
  mutate(ethnicity = as_character(ethnicity), 
         ethnic = ethnicity
  )

