# data preparation file for malawi
#extract data
malawi <- bind_rows(df$malawi_2000, df$malawi_2004, df$malawi_2010, df$malawi_2016)

malawi %<>%
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

malawi %<>% 
  mutate(country_name = "Malawi")

# harmonize religion
malawi %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"), 
                            "Protestant/other christian" = c("anglican", "ccap", "other christian", "seventh day advent./baptist",
                                                             "seventh day advent/ baptist", "seventh day adventist / baptist",
                                                             "seventh day adventist/baptist"),
                            "Muslim" = c("muslim"),
                            "Other" = c("no religion" , "other", "")
  ))

# harmonize ethnicity
malawi %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "chewa" = c("chewa"), 
                               "lomwe" = c("lomwe"),
                               "ngoni" = c("ngoni"),
                               "nkhonde" = c("nkhonde", "nkonde"), 
                               "sena" = c("sena"), 
                               "tonga" = c("tonga"),
                               "tumbuka" = c("tumbuka"),
                               "yao" = c("yao"),
                               "other" = c("other", "other: nyanja", "other: lambya", "other: mang'anja", "other: ndali", "amanganja/anyanja", "")
  ))


