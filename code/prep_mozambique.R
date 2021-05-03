# data preparation file for mozambique
#extract data
mozambique <- bind_rows(df$mozambique_1997, df$mozambique_2003, df$mozambique_2011, df$mozambique_2015)

mozambique %<>%
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

mozambique %<>% 
  mutate(country_name = "Mozambique")

# harmonize religion
mozambique %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "catholic (cat¢lica)"), 
                            "Protestant/other christian" = c("anglican", "evangelical/pentecostal", "protestant", "protestant / evangelic", "zion", "zionist"),
                            "Muslim" = c("islamic", "islamic (mu‡ulman)", "moslem"),
                            "Other" = c("animist" , "other", "other (outra)", "no religion", "no religion (sem religiao)", "")
  ))

# harmonize ethnicity
mozambique %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Emakhuwa" = c("emakhuwa", "emakua & simili"),
                               "Portuguese" =  c("portugu?s","portugues"),
                               "Xichangana" = c("xichangana"),
                               "Chichewa" = c("cicewa"),
                               "Cisena" = c("cisena", "cisena & simili", "cibalke"),
                               "Elomwe" = c("elomue & emarenjo", "elomwe"),
                               "Echuwabo" = c("echuwabo"),
                               "Cindau" = c("cindau"),
                               "Xitsonga" =  c("bitonga","xirhonga","xitsonga & simili","xitswa & simili", "xitswa"),
                               "Shona" = c("chitewe","shona"),
                               "Other" =  c("", "cichopi", "cinyungwe", "ciyao", "coti", "kimwane", "missing", "other", "outra", "shimakonde")
                               ))

# problem: no ethnicities for 2003 and 2015
