# this script builds models to explore equity of access to community services after discharge from hospital
# this script needs to be run in the NCDR data science environment

# 0 set_up ----

library(tidyverse)
library(here)
library(broom)
library(mgcv)


# 1 load model data, models and results ----
modDf <- readRDS(here('dataRDS', 'modDf.RDS'))

modPrelim <- readRDS(here('dataRDS', 'modPrelim.RDS'))
modPrelim_Results <- readRDS(here('dataRDS', 'modPrelim_Results.RDS'))

mod <- readRDS(here('dataRDS', 'mod.RDS'))
mod_Results <- readRDS(here('dataRDS', 'mod_Results.RDS'))

modSens1 <- readRDS(here('dataRDS', 'modSens1.RDS'))

modSens2 <- readRDS(here('dataRDS', 'modSens2.RDS'))

modDfSens3 <- readRDS(here('dataRDS', 'modDfSens3.RDS'))
modSens3 <- readRDS(here('dataRDS', 'modSens3.RDS'))

sens_Results <- saveRDS(, here('dataRDS', 'sens_Results.RDS'))


modDf_fracFemur <- readRDS(here('dataRDS', 'modDf_fracFemur.RDS'))
mod_fracFemur <- readRDS(here('dataRDS', 'mod_fracFemur.RDS'))
mod_fracFemur_Results <- readRDS(here('dataRDS', 'mod_fracFemur_Results.RDS'))


modDf_frailtyFemur <- readRDS(here('dataRDS', 'modDf_frailtyFemur.RDS'))
mod_frailtyFemur <- readRDS(here('dataRDS', 'mod_frailtyFemur.RDS'))
mod_frailtyFemur_Results <- readRDS(here('dataRDS', 'mod_frailtyFemur_Results.RDS'))


# 2 chart preliminary model results ----

modPrelim_Results %>% 
  bind_rows(data.frame(term = c('imdQF1', 'sexMfmale', 'ethnicityWhite - British', 'icbShortNameCambridgeshire & Peterborough', 'ageGrp65-69y', 
                                'loSGrp00d', 'cciGrp1_2c', 'specGrp300', 'podGrpnonElective', 'priorCommContactAttF2FGrp00p' ),
                       estimate = rep(0, 10),
                       std.error = rep(NA_real_, 10),
                       statistics = rep(NA_real_, 10),
                       p.value = rep(NA_real_, 10))) %>% 
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             substr(term, 1, 3) == 'icb' ~ 'ICB',
                             substr(term, 1, 3) == 'age' ~ 'Age group',
                             substr(term, 1, 3) == 'loS' ~ 'LoS group',
                             substr(term, 1, 3) == 'cci' ~ 'Comorbidity group',
                             substr(term, 1, 3) == 'spe' ~ 'Specialty',
                             substr(term, 1, 3) == 'pod' ~ 'Point of delivery',
                             substr(term, 1, 3) == 'pri' ~ 'Prior Community Contacts',
                             substr(term, 1, 3) == '(In' ~ 'intercept',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp  != 'intercept') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or)) +
  scale_x_log10(name ='Odds ratio (log scale)') +
  facet_wrap(~termGrp, scales = 'free')



# 3 chart main model results ----

mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(0, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>%  
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             substr(term, 1, 3) == 'icb' ~ 'ICB',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity', 'ICB')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +
  scale_x_log10(name ='Odds ratio (log scale)') +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

plot(mod, residuals = FALSE, se = FALSE, select = 1)
plot(mod, residuals = FALSE, se = FALSE, select = 2)
plot(mod, residuals = FALSE, se = FALSE, select = 3)
plot(mod, residuals = FALSE, se = FALSE, select = 4)



# 3a create chart outputs for covariates of interest ----
mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(0, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>%  
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             substr(term, 1, 3) == 'icb' ~ 'ICB',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity', 'ICB')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  filter(termGrp == 'Ethnicity') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'red')) +
  # facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreEthnicityORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)

mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(0, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>%  
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             substr(term, 1, 3) == 'icb' ~ 'ICB',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity', 'ICB')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  filter(termGrp == 'IMD') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'red')) +
  # facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreDeprivationORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)


mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(0, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>%  
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             substr(term, 1, 3) == 'icb' ~ 'ICB',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity', 'ICB')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  filter(termGrp == 'ICB') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  # facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreIcbORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)


mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(0, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>%  
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>%  
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             substr(term, 1, 3) == 'icb' ~ 'ICB',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity', 'ICB')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  filter(termGrp == 'Sex') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.96, 0.98, 1.0)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'red')) +
  # facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreSexORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)



# 3 create chart outputs for sensitivity analysis----
sens_Results %>% 
  filter(termGrp == 'IMD') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = mod, size = mod)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.85, 0.9, 0.95, 1.0)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('black', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

ggsave(here('charts', 'plotSensDeprivationORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)

sens_Results %>% 
  filter(termGrp == 'Ethnicity') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = mod, size = mod)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('black', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

ggsave(here('charts', 'plotSensEthnicityORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)

sens_Results %>% 
  filter(termGrp == 'Sex') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = mod, size = mod)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.96, 0.98, 1.0)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('black', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

ggsave(here('charts', 'plotSensSexORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)

sens_Results %>% 
  filter(termGrp == 'ICB') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = or, colour = mod, size = mod)) +
  scale_x_log10(name ='Odds ratio (log scale)',
                breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('black', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

ggsave(here('charts', 'plotSensICBORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 15,
       units = 'cm',
       dpi = 300)


# 4 create charts for subgroup analysis----
# fractured femur

mod_fracFemur_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityGrpWhite (ref)'),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       statistics = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3))) %>% 
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +  
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  scale_x_log10(name ='Odds ratio (log scale)') +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')




mod_frailty_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityGrpWhite (ref)'),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       statistics = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3))) %>% 
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               substr(term, 1, 3) == 'age' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'loS' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'cci' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'spe' ~ substr(term, 8, nchar(term)),
                               substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               substr(term, 1, 3) == 'pri' ~ substr(term, 26, nchar(term)),
                               substr(term, 1, 3) == '(In' ~ 'intercept',
                               TRUE ~ 'check')) %>% 
  filter(termGrp %in% c('IMD', 'Sex', 'Ethnicity')) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +  
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  scale_x_log10(name ='Odds ratio (log scale)') +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

## create simpe model - all patients
## create output charts for subgroup analysis
