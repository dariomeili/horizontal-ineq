# data preparation file for senegal
#extract data
senegal <- bind_rows(df$senegal_1993, df$senegal_1997, df$senegal_2005, df$senegal_2011, df$senegal_2014, df$senegal_2016)

senegal %<>%
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

senegal %<>% 
  mutate(country_name = "Senegal")

# harmonize religion
senegal %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("christian", "Christian"), 
                            "Muslim" = c("muslim", "Muslim"),
                            "Other" = c("animist", "Animist", "no religion", "No religion", "Other", "")
  ))

# harmonize ethnicity
senegal %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Diola" = c("Diola" , "diola" , "diola"),
                               "Soninke" = c("Soninke", "soninké", "sarakole /soninke", "sarakhol‚, soninke", "soninke" , "soninké/sarakolé"),
                               "Wolof" = c("Wolof" , "wolof" , "wolof /lebou"),
                               "Mandingue" = c("mandingue", "Mandingue" , "mandingue /soce/malnk" , "Mandingue", " mandingue/socé"),
                               "Serer" = c("Serer", "serer"),
                               "Poular" = c("Poular" , "poular"),
                               "other" = c( "bambara", "balant", "mancagne", "manjaak", "Other", "other", "Not a Senegalese" , "not a senegalese" , "not senegalese" , "other senegalese" , "other,non-senegalese", "")
                               ))




