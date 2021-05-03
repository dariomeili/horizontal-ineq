library(tidyverse)
library(magrittr)
library(sjmisc)
library(knitr)
library(kableExtra)
library(modelsummary)
# load data
load("data/prep_all.R")

# source functions
source("code/functions.R")
source("code/ggini.R")

# nest data frame
df_nest <- all %>% 
  group_nest(country_name, year)

# generate intersections with intersections 
## gender x religion
df_nest %<>%
  mutate(data = map(data, ~intersect(., gender, rel, .id = "gen_rel")))
## gender x ethnicity 
df_nest %<>%
  mutate(data = map(data, ~intersect(., gender, ethnic, .id = "gen_eth")))
## gender x residence 
df_nest %<>%
  mutate(data = map(data, ~intersect(., gender, residence, .id = "gen_res")))

# compute ginis for pure groups
## gender
df_nest %<>%
  mutate(res_gen = map(data, ~decomp(., outcome = education, grouping = gender, weights = wgt)))

## religion
df_nest %<>%
  mutate(res_rel = map(data, ~decomp(., outcome = education, grouping = rel, weights = wgt))) 

## ethnicity
df_nest %<>%
  mutate(res_eth = map(data, ~decomp(., outcome = education, grouping = ethnic, weights = wgt))) 

## residence
df_nest %<>%
  mutate(res_res = map(data, ~decomp(., outcome = education, grouping = residence, weights = wgt))) 

# compute ginis for intersected groups
## gender x religion
df_nest %<>%
  mutate(res_gen_rel = map(data, ~decomp(., outcome = education, grouping = gen_rel, weights = wgt)))

## gender x ethnicity
df_nest %<>%
  mutate(res_gen_eth = map(data, ~decomp(., outcome = education, grouping = gen_eth, weights = wgt)))

## gender x residence 
df_nest %<>%
  mutate(res_gen_res = map(data, ~decomp(., outcome = education, grouping = gen_res, weights = wgt)))

df_nest %<>%
  mutate(gg_gen= map(.$res_gen, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist(),
         gg_rel = map(.$res_rel, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist(),
         gg_eth = map(.$res_eth, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist(),
         gg_res = map(.$res_gen, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist(),
         gg_gen_rel = map(.$res_gen_rel, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist(),
         gg_gen_eth = map(.$res_gen_eth, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist(),
         gg_gen_res = map(.$res_gen_res, "gini_decomp") %>% 
           map(., "gini_between") %>% 
           unlist()) 

# add id
df_nest %<>% 
  mutate(id = 1:nrow(df_nest))

# save data
save(df_nest, file = "data/master.Rda")

ggplot(df_nest, aes(x=year, y=gg_gen)) +
  geom_point() 
 
ggplot(df_nest, aes(x=year, y=gg_rel)) +
  geom_point()  

ggplot(df_nest, aes(x=year, y=gg_eth)) +
  geom_point()

ggplot(df_nest, aes(x=gg_gender, y=gg_gen_rel)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(df_nest, aes(x=gg_rel, y=gg_gen_rel)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(df_nest, aes(x=gg_gender, y=gg_gen_eth)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(df_nest, aes(x=gg_eth, y=gg_gen_eth)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(df_nest, aes(x=gg_gen_eth, y=gg_gen_rel)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0)

ggplot(df_nest, aes(x = year, y = country_name, colour = gg_gen, size = gg_gen)) +
  geom_point() +
  scale_colour_viridis_c(option = "magma", direction = -1)

ggplot(df_nest, aes(x = year, y = country_name, colour = gg_gen_rel, size = gg_gen_rel)) +
  geom_point() +
  scale_colour_viridis_c(option = "magma", direction = -1)




df_path %<>% 
  mutate(d_year = year.new - year.old,
         d_gen = gg_gen.new - gg_gen.old,
         d_rel = gg_rel.new - gg_rel.old, 
         d_eth = gg_eth.new - gg_eth.old,
         d_res = gg_res.new - gg_eth.old,
         d_gen_rel = gg_gen_rel.new - gg_gen_rel.old,
         d_gen_eth = gg_gen_eth.new - gg_gen_eth.old,
         d_gen_res = gg_gen_res.new - gg_gen_res.old,
         r_gen = d_gen/d_year,
         r_rel = d_rel/d_year,
         r_eth = d_eth/d_year,
         r_res  = d_res/d_year,
         r_gen_rel = d_gen_rel/d_year, 
         r_gen_eth = d_gen_eth/d_year,
         r_gen_res = d_gen_res/d_year
         )

df_path %>%
  pivot_longer(cols = r_gen:r_gen_res) %>% 
  ggplot(aes(x=country_name, y=value)) +
  geom_col() +
  coord_flip() +
  facet_wrap(.~name)
