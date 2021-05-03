# data preparation file for congo_dr
# filter observations with education above 30 years and column hv024 because R throws an error when trying to bind datasets
df$congo_dr_2007 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)
df$congo_dr_2014 %<>% 
  filter(v133 <= 30) %>% 
  select(-hv024)

#extract data
congo_dr <- bind_rows(df$congo_dr_2007, df$congo_dr_2014)
congo_dr %<>%
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

congo_dr %<>% 
  mutate(country_name = "Democratic Republic of the Congo")


congo_dr %<>%
  mutate(rel = fct_collapse(religion, 
                            "Catholic" = c("catholic"),
                            "Protestant/other christian" = c("armée de salut", 
                                                             "kimbanguist",
                                                             "kimbanguiste",
                                                             "other christian",
                                                             "other christians",
                                                             "protestant",
                                                             "salvation army"),
                            "Muslim" = c("muslim"), 
                            "Traditionalist" = c("animist"),
                            "Other" = c("no religion", "bundu dia kongo", "other", "vuvamu", "")
  ))

# harmonize ethnicity
congo_dr %<>% 
  mutate(ethnic = fct_collapse(ethnicity,
                               "bakongo nord & sud" = c("bakongo nord & sud", "bakongo north and south"),
                               "bas-kasai and kwilu-kwngo" = c("bas-kasai and kwilu-kwngo", "bas-kasai et kwilu-kwngo"),
                               "basele-k , man. and kivu" = c("basele-k , man. and kivu",  "basele-k , man. et kivu"), 
                               "uele lac albert" = c("uele lac albert",  "uele lake albert"),
                               "ubangi et itimbiri" = c( "ubangi et itimbiri", "ubangi and itimbiri"), 
                               "cuvette central" = c("cuvette central"),
                               "kasai, katanga, tanganika" = c("kasai, katanga, tanganika"), 
                               "pygmy" = c("pygmy"),  
                               "lunda" = c("lunda"),
                               "other" = c("other", "others", "foreign/non-congolese"))
  )
