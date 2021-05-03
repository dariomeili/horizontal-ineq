# data preparation file for zambia
#extract data
zambia <- bind_rows(df$zambia_1996, df$zambia_2002, df$zambia_2007, df$zambia_2014)

zambia %<>%
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
zambia %<>% 
  mutate(country_name = "Zambia")

# harmonize religion
zambia %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"), 
                            "Protestant/other christian" = c("protestant"),
                            "Muslim" = c("muslim"),
                            "Other" = c("", "none", "other", "no religion")
  ))

# harmonize ethnicity
zambia %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Bemba" = c("bemba"),   
                               "Bisa" = c("bisa"),  
                               "Chewa" = c("chewa"),  
                               "Lala" = c("lala"),
                               "Lamba" = c("lamba"), 
                               "Mambwe" = c("mambwe"), 
                               "Mbunda" = c("mbunda"), 
                               "Namwanga" = c("namwanga"), 
                               "Tumbuka" = c("tumbuka"), 
                               "Lunda" = c("lunda (luapula)", "lunda (north-western)", "lunda (northwestern)"),
                               "Lozi" = c("lozi"), 
                               "Ushi" = c("ushi"), 
                               "Nsenga" = c("nsenga"), 
                               "Lenje" = c("lenje"), 
                               "Luchazi" = c("luchazi"), 
                               "Senga" = c("senga"), 
                               "Chishinga" = c("chishinga"), 
                               "Kunda" = c("kunda"), 
                               "Chokwe" = c("chokwe"), 
                               "Kabende" = c("kabende"), 
                               "Bwile" = c("bwile"), 
                               "Ila" = c("ila"), 
                               "Kunda" = c("kunda"),
                               "Lungu" = c("lungu"), 
                               "Luvale" = c("luvale"), 
                               "Mashi" = c("mashi"), 
                               "Ngoni" = c("ngoni"), 
                               "Ngumbo" = c("ngumbo"),
                               "Nkoya" = c("nkoya"), 
                               "Nyanja" = c("nyanja"), 
                               "Nyengo" = c("nyengo"),
                               "Tonga" = c("tonga"),
                               "Soli" = c("soli"), 
                               "Tabwa" = c("tabwa"), 
                               "Toka-leya" = c("toka-leya"), 
                               "Unga" = c("unga"), 
                               "Totela" = c("totela"), 
                               "Kwanga" = c("kwanga", "kwangwa"),
                               "Sala" = c("sala"),
                               "Swawka" = c("swawka", "swaka"),
                               "Subiya" = c("subiya"),
                               "Other" = c("african", "ambo" , "american" , "asian" , "chicunda" , "dk", "english" , "european",
                                           "gowa" , "imilangu" , "koma" , "kwandi", "lima","luano", "luyana", "mashasha", "mbowe", 
                                           "mukulu", "mwenyi", "other", "nwenyi", "other african", "other zambia", "yombe", 
                                           "other zambian", "shila", "simaa", "subiya", "tambo", "other language", "chikunda", 
                                           "kaonde", "") 
                               )) %>% 
  mutate(ethnic = fct_lump(ethnic, prop = 0.025))



