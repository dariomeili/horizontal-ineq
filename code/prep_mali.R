# data preparation file for mali
#extract data
mali <- bind_rows(df$mali_2006, df$mali_2013)

mali %<>%
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

mali %<>% 
  mutate(country_name = "Mali")

# harmonize religion
mali %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("catholic", "christian", "christrian", "evangelical", "methodist", "other christian"),
                            "Muslim" = c("muslim"),
                            "Other" = c("animist", "no religion", "other","")
  ))

# harmonize ethnicity
mali %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "bambara" = c("bambara", "bambara." , "Bambara."), 
                               "malinke" = c("malinke", "malink," , "malinké" , "Malinke", "malink‚"),
                               "sonrai" = c("sonrai",  "sonra,",  "sonraï" , "Sonraï", "sonra‹"),
                               "senoufo/minianka" = c("s,noufo /minianka",  "senoufo/minianka" ,  "senoufo/minianka", "sénoufo/minianka", "Sénoufo/minianka", "s‚noufo /minianka"),
                               "sarkole/soninke/marka"  = c("sarakole/soninke/marka", "sarakol, /sonink,",  "sarkolé/soninké/marka", "Sarakole/soninke/marka", "sarakol‚ /sonink‚"),
                               "bobo" = c("bobo", "Bobo"),
                               "tamacheck" = c("tamacheck", "tamachek/bélla", "tanachek", "Tamachek/bélla"),
                               "peulh" = c("peulh" , "Peulh"),
                               "dogon" = c("dogon" , "Dogon"),
                               "other" = c("cdeao country" , "other nationalities", "ecowas countries", "other", "foreigner", "other african countries", "other african country", "other countries"  , "other ethny", "CDEAO Country" , "Other African Country", "Other nationalities", "Other", "")
  ))


