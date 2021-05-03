# data preparation file for sierra_leone
#extract data
sierra_leone <- bind_rows(df$sierra_leone_2008, df$sierra_leone_2013)

sierra_leone %<>%
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

sierra_leone %<>% 
  mutate(country_name = "Sierra Leone")

# harmonize religion
sierra_leone %<>%
  mutate(rel = fct_collapse(religion, 
                            "Christian" = c("christian"), 
                            "Muslim" = c("islam"),
                            "Other" = c("bahai", "none", "other", "traditional", "")
  ))

# harmonize ethnicity
sierra_leone %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Kono" = c("kono","Kono"),
                               "Mende" = c("mende","Mende","mande"),
                               "Limba" = c("limba","Limba"),
                               "Loko" = c("loko" ,"Loko"),
                               "Temne" = c("temne" ,"Temne"),
                               "Mandingo" = c("mandingo" ,"Mandingo" ,"madingo"),
                               "Sherbro" = c("sherbro","Sherbro"),
                               "Other" = c( "other sierra leone","other foreign","other non sierra leone","kriole" ,"koranko","creole","fullah","Fullah","Koranko","Creole","Other Sierra Leone","Other Foreign", "")
                               ))





