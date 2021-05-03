# data preparation file for uganda
#extract data
uganda <- bind_rows(df$uganda_1995, df$uganda_2001, df$uganda_2006, df$uganda_2011, df$uganda_2016)

uganda %<>%
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

uganda %<>% 
  mutate(country_name = "Uganda")

# harmonize religion
uganda %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"), 
                            "Protestant/other christian" = c("anglican", "baptist", "jehovah's witness", "mammon", "orthodox", "pentecostal",
                                                             "pentecostal/born again/evangelical", "presbyterian", "protestant", "salvation army",
                                                             "sda", "seventh day advent.", "seventh day adventist"),
                            "Muslim" = c("muslim"),
                            "Other" = c("", "baha'i", "hindu", "jewish", "no religion", "traditional", "other")
  ))

# harmonize ethnicity
uganda %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Acholi" = c("acholi", "adja-ewe/mina"),  
                               "Alur" = c("alur", "akposso/akebou", "jonam"), 
                               "Atesa" = c("atesa", "iteso"), 
                               "Baganda" = c("baganda"),   
                               "Bakiga" = c("bakiga"),
                               "Banyankole" = c("banyankole", "banyankore"),
                               "Basoga" = c("basoga"), 
                               "Langi" = c("langi", "lango"),
                               "Lugbara" = c("lugbara", "aringa", "kebu (okebu)"), 
                               "Madi" = c("madi"), 
                               "Mufumbira" = c("mufumbira", "bafumbira"),
                               "Mugwere" = c("mugwere", "bagwere"),
                               "Mukonjo" = c("mukonjo", "bakonjo", "bakonzo"),
                               "Munyarwanda" = c("munyarwanda", "banyarwanda"),
                               "Munyoro" = c("munyoro", "banyoro"),
                               "Mutooro" = c("mutooro", "batoro"),
                               "Ngakaramajong" = c("ngakaramajong", "karimojong"),
                               "Other" = c("other", "baamba", "bachope", "bagisu", "bagwe", "bahororo", "banyole", "barulli", 
                                           "barundi", "samia", "nubiam", "other", "kumam", "sebei", "lendu", "batwa", "kakwa", 
                                           "lendu", "mugishu", "badama", "", "aliba", "babukusu", "babwisi", "bagungu", "bahehe", "bakenyi", "banyabindi",
                                           "banyabutumbi", "banyara", "banyaruguru", "baruli", "basamia", "basongora", "batagwenda", "batuku", "chope", "dodoth",
                                           "ethur", "gimara", "ik (teuso)", "jie", "jopadhola", "kuku", "mening", "napore", "nubi", "pokot", "reli", "sabiny", "so (tepeth)")
                               ))



