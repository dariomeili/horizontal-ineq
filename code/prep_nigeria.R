# data preparation file for nigeria
#extract data
nigeria <- bind_rows(df$nigeria_1999, df$nigeria_2003, df$nigeria_2008, df$nigeria_2013)

nigeria %<>%
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

nigeria %<>% 
  mutate(country_name = "Nigeria")

# harmonize religion
nigeria %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"), 
                            "Protestant/other christian" = c("protestant", "other christian"),
                            "Muslim" = c("islam"),
                            "Other" = c("traditionalist", "other", "")
  ))

# harmonize ethnicity
nigeria %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "ekoi" = c("ekoi"), 
                               "fulani" = c("fulani", "fulfulde"),
                               "hausa" = c("hausa"),
                               "ibibio" = c("ibibio"),
                               "igala" = c("igala"), 
                               "igbo" = c("igbo", "delta ibo", "igbo/ibo"), 
                               "ijaw/izon" = c("ijaw/ izon", "ijaw/izon"),
                               "kanuri/beriberi" = c("kanuri/beriberi", "kanuri/ beriberi"),
                               "tiv" = c("tiv"),
                               "yoruba" = c("yoruba", "egba"),
                               other_level = "other"
  ))


