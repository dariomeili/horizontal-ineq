library(tidyverse)
library(magrittr)
library(sjmisc)
library(knitr)
library(kableExtra)
library(modelsummary)
library(dineq)
# load data
load("data/prep_all.R")

# source functions
source("functions/functions.R")
source("functions/ggini.R")

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
  mutate(res_gen = map(data, ~ggini(., outcome = education, grouping = gender, weights = wgt)))

## religion
df_nest %<>%
  mutate(res_rel = map(data, ~ggini(., outcome = education, grouping = rel, weights = wgt))) 

## ethnicity
df_nest %<>%
  mutate(res_eth = map(data, ~ggini(., outcome = education, grouping = ethnic, weights = wgt))) 

## residence
df_nest %<>%
  mutate(res_res = map(data, ~ggini(., outcome = education, grouping = residence, weights = wgt))) 

# compute ginis for intersected groups
## gender x religion
df_nest %<>%
  mutate(res_gen_rel = map(data, ~ggini(., outcome = education, grouping = gen_rel, weights = wgt)))

## gender x ethnicity
df_nest %<>%
  mutate(res_gen_eth = map(data, ~ggini(., outcome = education, grouping = gen_eth, weights = wgt)))

## gender x residence 
df_nest %<>%
  mutate(res_gen_res = map(data, ~ggini(., outcome = education, grouping = gen_res, weights = wgt)))

# write ggini values from results to separate columns
df_nest %<>%
  mutate(gg_gen= map(.$res_gen, "group_gini") %>% 
           unlist(),
         gg_rel = map(.$res_rel, "group_gini") %>% 
           unlist(),
         gg_eth = map(.$res_eth, "group_gini") %>% 
           unlist(),
         gg_res = map(.$res_gen, "group_gini") %>% 
           unlist(),
         gg_gen_rel = map(.$res_gen_rel, "group_gini") %>% 
           unlist(),
         gg_gen_eth = map(.$res_gen_eth, "group_gini") %>% 
           unlist(),
         gg_gen_res = map(.$res_gen_res, "group_gini") %>% 
           unlist()) 

# compute max gginis
source("functions/max_ineq.R")

# compute max counterfactual ginis for pure groups
## max gender
df_nest %<>%
  mutate(res_gen.max = map(data, ~max_ineq(., outcome = education, grouping = gender, weights = wgt)))

## max religion
df_nest %<>%
  mutate(res_rel.max = map(data, ~max_ineq(., outcome = education, grouping = rel, weights = wgt))) 

## ethnicity
df_nest %<>%
  mutate(res_eth.max = map(data, ~max_ineq(., outcome = education, grouping = ethnic, weights = wgt))) 

## residence
df_nest %<>%
  mutate(res_res.max = map(data, ~max_ineq(., outcome = education, grouping = residence, weights = wgt))) 

# compute max counterfactual ginis for intersected groups
## gender x religion
df_nest %<>%
  mutate(res_gen_rel.max = map(data, ~max_ineq(., outcome = education, grouping = gen_rel, weights = wgt)))

## gender x ethnicity
df_nest %<>%
  mutate(res_gen_eth.max = map(data, ~max_ineq(., outcome = education, grouping = gen_eth, weights = wgt)))

## gender x residence 
df_nest %<>%
  mutate(res_gen_res.max = map(data, ~max_ineq(., outcome = education, grouping = gen_res, weights = wgt)))

# write max counterfactuel ggini values from results to separate columns
df_nest %<>%
  mutate(max_gen= map(.$res_gen.max, "group_gini") %>% 
           unlist(),
         max_rel = map(.$res_rel.max, "group_gini") %>% 
           unlist(),
         max_eth = map(.$res_eth.max, "group_gini") %>% 
           unlist(),
         max_res = map(.$res_gen.max, "group_gini") %>% 
           unlist(),
         max_gen_rel = map(.$res_gen_rel.max, "group_gini") %>% 
           unlist(),
         max_gen_eth = map(.$res_gen_eth.max, "group_gini") %>% 
           unlist(),
         max_gen_res = map(.$res_gen_res.max, "group_gini") %>% 
           unlist()) 

# add id
df_nest %<>% 
  mutate(id = 1:nrow(df_nest))

df_nest %<>% 
  mutate(across(res_gen:res_gen_res, ~ .x %>% 
                  map("group_means") %>% 
                  map(~ineq_ratio(.)),
                .names = "{.col}_ratio"
  ))

# save data
save(df_nest, file = "data/master.Rda")


