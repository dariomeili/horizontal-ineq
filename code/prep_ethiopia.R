# data preparation file for ethiopia
#extract data
ethiopia <- bind_rows(df$ethiopia_2000, df$ethiopia_2005, df$ethiopia_2011)

ethiopia %<>%
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
         region = region_3, 
         residence = hv025, 
         gender = gender, 
         age = v012, 
         religion = religion, 
         ethnicity = ethnicity,
         education = v133, 
         id
  )

ethiopia %<>% 
  mutate(country_name = "Ethiopia")


ethiopia %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"),
                            "Protestant/other christian" = c("protestant", "orthodox"),
                            "Muslim" = c("moslem", "muslim"), 
                            "Traditionalist" = c("traditional"),
                            "Other" = c("other")
  ))

# harmonize ethnicity
ethiopia %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "Affar" = c("affar", "affar / adal, danakil, denkel"),
                               "Amhara" = c("amara / gondere, gayente, semen, farte, gojjame, dawunte, wa",
                                            "amhara",
                                            "amharra"),
                               "Guragie" = c("guragie",
                                             "gurarie / cheha, ener, enemor, ezaya, gumer, gura, megareb,"),
                               "Hadiya" = c("hadiya",
                                            "silte"),
                               "Oromo" = c("oromo",
                                           "oromo / guji, borena, tulema, kereyu, gelan, lika, weredube,"),
                               "Sidama" = c("sidama"),
                               "Somali" = c("somalie"),
                               "Tigray" = c("tigraway / tigre",
                                            "tigray (tigraway)",
                                            "tigrie"),
                               "Welaita" = c("welaita"),
                               other_level = "Other"
                               )
                               )

   
