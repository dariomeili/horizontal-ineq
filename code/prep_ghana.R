# data preparation file for ghana
#extract data
ghana <- bind_rows(df$ghana_1993, df$ghana_1998, df$ghana_2003, df$ghana_2008, df$ghana_2014)

ghana %<>%
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

ghana %<>% 
  mutate(country_name = "Ghana")

# harmonize religion
ghana %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "roman catholic"),
                            "Protestant/other christian" = c("anglican" , "methodist", "presbyterian", "pentecostal/charismatic", "other christian", "protestant"),
                            "Muslim" = c("islam", "moslem" , "muslim"),
                            "Traditionalist" = c("traditional/spiritualist", "traditional" , "spiritualist"),
                            "Other" = c("no religion" , "other" , "no religion + other", "")
  ))

# harmonize ethnicity
ghana %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "akan" = c("akwapim", "asante" , "fante", "fanti", "other akan", "akan"),
                               "ga/dangme" = c("ga.adangbe", "ga/adangbe", "ga/dangme"), 
                               "ewe" = c("ewe"), 
                               "mole-dagbani" = c("mole-dagbani"), 
                               "gruma" = c("gruma", "gurma"),
                               "grussi" = c("grusi", "grussi"), 
                               "guan" = c("guan"),
                               other_level = "Other"
  ))

