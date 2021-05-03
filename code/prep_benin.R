# data preparation file for benin

#extract data
benin <- bind_rows(df$benin_2001, df$benin_2012)
benin %<>%
  select(country_name, 
         year = survey_year,
         line_number = hvidx,
         cluster_id = hv001, 
         household_id = hv002,
         respondent_line = hv003, 
         wgt = hv005,
         sampling_unit = hv021, 
         stratification= hv025, 
         region = region_3, 
         residence = hv025, 
         wealth = hv271, 
         gender = gender, 
         age = v012, 
         religion = religion, 
         ethnicity = ethnicity,
         education = v133, 
         id
         )
# making sure country names are right
benin %<>% 
  mutate(country_name = "Benin")

benin %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("Catholic",
                                           "catholic",
                                           "Christian"),
                            "Protestant/other christian" = c("celestes",   
                                           "celestes (celestial church of christ)",
                                           "methodist",
                                           "other christian",
                                           "other christians",
                                           "other protestant",
                                           "protestant",
                                           "protestant methodist",
                                           "protestants",
                                           "Methodist",
                                           "Other Protestant",
                                           "Celestes (Celestial Church of Christ)",
                                           "Other Christian",
                                           "protestant (protestante)"),
                            "Muslim" = c("islam",
                                         "islamic",
                                         "moslem",
                                         "muslim",
                                         "moslem",
                                         "Islam"), 
                            "Other" = c("no religion",
                                        "none",
                                        "other religion",
                                        "other religions",
                                        "other",
                                        "Other religion",
                                        "No religion",
                                        "other traditional",
                                        "taditional",
                                        "traditional",
                                        "traditional (vodoun)",
                                        "vodoun",
                                        "Vodoun",
                                        "Other traditional", 
                                        "")
                            ))

# hormonize ethnicity
benin %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Adja" = c("adja", "adja & related", "adja and related", "Adja"),
                               "Bariba" = c("bariba", "bariba & related", "bariba and related", "Bariba"),
                               "Betamaribe" = c("betamaribe", "betamaribe & related", "bétamaribe and related", "btamaribe and related", "Betamaribe"),
                               "Dendi" = c("dendi", "dendi & related", "dendi and related", "Dendi"),
                               "Fon" = c("fon", "fon & related", "fon and related", "Fon"),
                               "Peulh"= c("peulh", "peulh & related", "peulh and related", "Peulh"),
                               "Yoa/Lokpa" = c("yoa" , "yoa & lokpa", "yoa, lokpa & related" , "yoa and lokpa and related" , "Yoa"),
                               "Yoruba" = c("yoruba" , "yoruba & related", "yoruba and related", "Yoruba"),
                               other_level = "Other"
                               ))


