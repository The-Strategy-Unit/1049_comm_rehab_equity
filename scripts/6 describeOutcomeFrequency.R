# this script builds models to explore equity of access to community services after discharge from hospital
# this script needs to be run in the NCDR data science environment

# 0 set_up ----

library(tidyverse)
library(here)


# 1 load model data frame ----
modDf <- readRDS(here('dataRDS', 'modDf.RDS'))


# 2 high level description ----
modDf %>% 
  group_by(postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n))

# 3 break down by non-equity factors ----
modDf %>% 
  group_by(podGrp, postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'postCommContactAttF2FYN', values_from = 'n', names_prefix = 'commContact') %>% 
  mutate(p = commContact1 / (commContact0 + commContact1))

modDf %>% 
  group_by(cciGrp, postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'postCommContactAttF2FYN', values_from = 'n', names_prefix = 'commContact') %>% 
  mutate(p = commContact1 / (commContact0 + commContact1))


modDf %>% 
  group_by(loSGrp, postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'postCommContactAttF2FYN', values_from = 'n', names_prefix = 'commContact') %>% 
  mutate(p = commContact1 / (commContact0 + commContact1))

modDf %>% 
  group_by(ageGrp, postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'postCommContactAttF2FYN', values_from = 'n', names_prefix = 'commContact') %>% 
  mutate(p = commContact1 / (commContact0 + commContact1))


modDf %>% 
  group_by(priorCommContactAttF2FGrp, postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'postCommContactAttF2FYN', values_from = 'n', names_prefix = 'commContact') %>% 
  mutate(p = commContact1 / (commContact0 + commContact1))


tretSpefLookup <- read.csv(here('dataRaw', 'tretSpefLookup.csv'), header = FALSE) %>% 
  select(specCode = V1,
         specName = V2) %>% 
  bind_rows(data.frame(specCode = 'otherSpec',
                       specName = 'all other specialties'))


modDf %>% 
  group_by(specGrp, postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = 'postCommContactAttF2FYN', values_from = 'n', names_prefix = 'commContact') %>% 
  mutate(p = commContact1 / (commContact0 + commContact1)) %>% 
  left_join(tretSpefLookup, by = c('specGrp' = 'specCode')) %>% 
  arrange(-p) %>% 
  print(n = 47)
