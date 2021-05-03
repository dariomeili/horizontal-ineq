# data preparation file for namibia
#extract data
namibia <- bind_rows(df$namibia_2000, df$namibia_2007, df$namibia_2013)

namibia %<>%
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

namibia %<>% 
  mutate(country_name = "Namibia")

# harmonize religion
namibia %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("roman catholic"), 
                            "Protestant/other christian" = c("elcin", "protestant", "protestant/anglican", "seventh-day adventist"),
                            "Other" = c("no religion" , "other", "")
  ))

# harmonize ethnicity
namibia %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "afrikaans" = c("afrikaans"), 
                               "damara/nama" = c("damara/nama", "damara /nama"),
                               "herero" = c("herero"),
                               "english" = c("english"),
                               "oshivambo" = c("oshivambo", "oshiwambo"), 
                               "san" = c("san"), 
                               "caprivi languages" = c("caprivi languages" , "lozi"), 
                               "kavango languages" = c("kavango languages" , "kwangali"),
                               "other" = c("german" , "other", "tswana", "")
  ))

# no ethnicities for 2007 and 2013??

