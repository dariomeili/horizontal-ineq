# data preparation file for cameroon
# filter observations with education above 30 years and column hv024 because R throws an error when trying to bind datasets
# 1991 has no records on ethnicity
df$cameroon_1991 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv105)

df$cameroon_1998 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv105)

df$cameroon_2004 %<>%
  filter(v133 <= 30) %>% 
  select(-hv105)

df$cameroon_2011 %<>%
  filter(v133 <= 30) %>% 
  select(-hv105)

#extract data
cameroon <- bind_rows(df$cameroon_1991,
                      df$cameroon_1998, 
                      df$cameroon_2004,
                      df$cameroon_2011)
cameroon %<>%
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
  ) %>% 
  filter(year != 1991) # 1991 has no ethnicity

cameroon %<>% 
  mutate(country_name = "Cameroon")

cameroon %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"),
                            "Protestant/other christian" = c("other christian" , "prostestant", "protestant", "other christian", "other christians", 
                                                             "other protestant", "protestant methodist"), 
                            "Muslim" = c("moslem", "muslim"), 
                            "Other" = c( "animist",  "nature worship", "new religions (eglises rebeillees)" , "boudist, hindu",
                                         "no religion" , "none", "other religion", "none" , "other", "")
  ))

# hormonize ethnicity
cameroon %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Adamaoua-Oubangui" = c("adamaoua-oubangui", "  "),
                               "Arab-choa/Peulh/Haoussa/Kanuri" = c("arab-choa/peulh/haoussa/kanuri", "peulh", "arabe choa"),
                               "Bamilike/Bamoun" = c("bamilike/bamoun", "bamilike-central", "bamoun"),
                               "Bantoïde South-West" = c("bantoïde south-west", "sousou", "soussou"),
                               "Beti/Bassa/Mbam" = c("beti/bassa/mbam" , "beti", "bassa-bakoko"),
                               "Biu-Mandara" = c("biu-mandara" , "Toma"),
                               "Côtier/Ngoe/Oroko" = c("côtier/ngoe/oroko", "ngoe-oroko", "cptoer"),
                               "Biu-Mandara" = c("biu-mandara"),
                               "Grassfields" = c("grassfields"),
                               "Kako/Meka/Pygmé" = c("kako/meka/pygmé", "pygmee", "kako", "meka"),
                               "Other" = c("Other", "other", "african", "bafia", " ")
  ))





