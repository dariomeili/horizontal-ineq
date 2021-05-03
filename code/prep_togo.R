# data preparation file for togo
#extract data
togo <- bind_rows(df$togo_1998, df$togo_2014)

togo %<>%
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

togo %<>% 
  mutate(country_name = "Togo")

# harmonize religion
togo %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"), 
                            "Protestant/other christian" = c("adventist", "assembly of god", "baptist", "evangelical presbyterian", "jehovah witness",
                                                             "methodist", "other christian", "pentecotist", "protestant presbyterian, methodist"),
                            "Muslim" = c("islamic", "muslim"),
                            "Other" = c("no religion", "none", "other", "traditional", "traditional/animist", "")
  ))

# harmonize ethnicity

togo %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Adja-Ewe" = c("adja-ewe" , "adja-ewe/mina"),  
                               "Akposso-Akebou" = c("akposso, akebou", "akposso/akebou"), 
                               "Ana-ife" = c("ana-ife"),  
                               "Kabye/Tem" = c("kabye/tem", "kabye, tem"),  
                               "Para-gourma/Akan" = c("para-gourma, akan", "para-gourma/akan"),
                               "Other" = c("non togolease", "other", "other togolese", "stranger", "")
                               
  ))


