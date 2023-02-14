# this script builds models to explore equity of access to community services after discharge from hospital
# this script needs to be run in the NCDR data science environment

# 0 set_up ----

library(tidyverse)
library(here)


# 1 load model data frame ----
modDf <- readRDS(here('dataRDS', 'modDf.RDS'))


# 2 summarise study population in tabular form ----
# by patient characteristics ----
table1a <- modDf %>%
  group_by(sexMf) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(grp = 'sex') %>% 
  rename(category = sexMf) %>% 
  bind_rows(modDf %>%
      group_by(ageGrp) %>% 
      summarise(n = n()) %>% 
      mutate(p = n / sum(n)) %>% 
      mutate(grp = 'age group') %>% 
      rename(category = ageGrp)) %>% 
  bind_rows(modDf %>%
      group_by(imdQF) %>% 
      summarise(n = n()) %>% 
      mutate(p = n / sum(n)) %>% 
      mutate(grp = 'Deprivation quintile') %>% 
      rename(category = imdQF)) %>% 
  bind_rows(modDf %>%
      group_by(ethnicity) %>% 
      summarise(n = n()) %>% 
      mutate(p = n / sum(n)) %>% 
      mutate(grp = 'ethnicity') %>% 
      rename(category = ethnicity)) %>% 
  mutate(p100 = 100 * p) %>% 
  select(grp, category, n, p, p100)


# by clinical characteristics ----
table1b <- modDf %>%
  group_by(podGrp) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(grp = 'admission method') %>% 
  rename(category = podGrp) %>% 
  bind_rows(modDf %>%
              group_by(cciGrp) %>% 
              summarise(n = n()) %>% 
              mutate(p = n / sum(n)) %>% 
              mutate(grp = 'Charlson comorbidity score') %>% 
              rename(category = cciGrp)) %>% 
  bind_rows(modDf %>%
              group_by(loSGrp) %>% 
              summarise(n = n()) %>% 
              mutate(p = n / sum(n)) %>% 
              mutate(grp = 'length of stay') %>% 
              rename(category = loSGrp)) %>%   
  bind_rows(modDf %>%
              group_by(priorCommContactAttF2FGrp) %>% 
              summarise(n = n()) %>% 
              mutate(p = n / sum(n)) %>% 
              mutate(grp = 'Comm services contact before admission') %>% 
              rename(category = priorCommContactAttF2FGrp)) %>%   
  mutate(p100 = 100 * p) %>% 
  select(grp, category, n, p, p100)
  


# by specialty ----
tretSpefLookup <- read.csv(here('dataRaw', 'tretSpefLookup.csv'), header = FALSE) %>% 
  select(specCode = V1,
         specName = V2) %>% 
  bind_rows(data.frame(specCode = 'otherSpec',
                       specName = 'all other specialties'))

table1c <- modDf %>%
  group_by(specGrp) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  left_join(tretSpefLookup, by = c('specGrp' = 'specCode')) %>% 
  mutate(grp = 'specialty') %>%   
  rename(category = specName) %>% 
  mutate(p100 = 100 * p) %>% 
  select(grp, category, n, p, p100)
  
  
# by ICB ----
table1d <- modDf %>%
  group_by(icbShortName) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n)) %>% 
  mutate(grp = 'Integrated Care Board') %>%   
  rename(category = icbShortName) %>% 
  mutate(p100 = 100 * p) %>% 
  select(grp, category, n, p, p100)


# 3 save files out as csv ----
write.csv(table1a, here('tables', 'table1a.csv'), row.names = FALSE)
write.csv(table1b, here('tables', 'table1b.csv'), row.names = FALSE)
write.csv(table1c, here('tables', 'table1c.csv'), row.names = FALSE)
write.csv(table1d, here('tables', 'table1d.csv'), row.names = FALSE)

# 4 other statistics ----
# median age

modDf %>% 
  summarise(medianAge = median(cci_Age))

modDf %>% 
  summarise(medianCCI = median(cci))



