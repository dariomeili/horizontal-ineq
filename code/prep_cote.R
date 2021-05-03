# data preparation file for cote
#extract data
cote <- bind_rows(df$cote_1999, df$cote_2012)

cote %<>%
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

cote %<>% 
  mutate(country_name = "Ivory Coast")


# harmonize religion
cote %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic", "Catholic"), 
                            "Protestant/other christian" = c("Evangelical", "Methodist", "Other Christian", "protestant"),
                            "Muslim" = c("islam", "muslim", "Muslim"),
                            "Other" = c("No religion" , "Other", "no religion" , "other", "no religion, traditional, other/missing", "Animist", "traditional", "")
  ))

# harmonize ethnicity
cote %<>% 
  mutate(ethnic = ethnicity)


