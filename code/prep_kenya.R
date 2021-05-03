# data preparation file for kenya
#extract data
kenya <- bind_rows(df$kenya_1993, df$kenya_1998, df$kenya_2003, df$kenya_2009, df$kenya_2014)

kenya %<>%
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

kenya %<>% 
  mutate(country_name = "Kenya")

# harmonize religion
kenya %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "roman catholic"), 
                            "Protestant/other christian" = c("protest /oth cristian", "protestant/ other christian", "protestant/other christian"),
                            "Muslim" = c("muslim"),
                            "Other" = c("no religion" , "other", "other religion", "")
  ))

# harmonize ethnicity
kenya %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "kalenjin" = c("kalenjin"),
                               "kamba" = c("kamba"), 
                               "kikuyu" = c("kikuyu", "kikuya"),
                               "taita/taveta" = c("taita/taveta", "taita /taveta", "taita/ taveta", "taita/tavate"),
                               "luo" = c("luo"),
                               "meru/embu" = c("meru/embu", "meru /embu", "meru", "embu"),
                               "mijikenda/swahili" = c("mijikenda/swahili" , "mijikenda /swahili", "mijikenda/ swahili"),
                               "somali" = c("somali"),
                               "luhya" = c("luhya"),
                               "kisii" = c("kisii"),
                               "other" = c("iteso", "pokomo", "turkana", "samburu", "rendille", "orma", "boran", "gabbra", "kuria", "mbere", "maasai", "masai", "")
                               ))


