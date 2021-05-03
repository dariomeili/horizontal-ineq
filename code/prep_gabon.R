# data preparation file for gabon
#extract data
gabon <- bind_rows(df$gabon_2000, df$gabon_2012)

gabon %<>%
  filter(v133 <= 30) %>% # filter out observations with more than 30 years of education
  mutate(country_name = "gabon") %>% 
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

gabon %<>% 
  mutate(country_name = "Gabon")

#harmonize religion
gabon %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"),
                            "Protestant/other christian" = c("protestant", "other christian"),
                            "Muslim" = c("muslim"), 
                            "Traditionalist" = c("animist"),
                            "Other" = c("other", "no religion", "atheist", "")
  ))

# harmonize ethnicity
gabon %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "fang" = c("fang"),
                               "kota-kele" = c("kota-kele"), 
                               "mbede-teke" = c("mbede-teke"), 
                               "myene" = c("myene"), 
                               "nzabi-duma" = c( "nzabi-duma"), 
                               "okande-tsogho" = c("okande-tsogho"), 
                               "pygmee" = c("pygmee"),
                               "shira-punu/vili" = c("shira-punu/vili"),  
                               "other" = c("foreign", "other", "")
  )
  )


