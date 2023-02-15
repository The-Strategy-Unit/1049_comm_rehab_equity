# this script builds models to explore equity of access to community services after discharge from hospital
# this script needs to be run in the NCDR data science environment

# 0 set_up ----

library(tidyverse)
library(here)
library(broom)
library(mgcv)


# 1 load model results ----
# modDf <- readRDS(here('dataRDS', 'modDf.RDS'))
# modPrelim <- readRDS(here('dataRDS', 'modPrelim.RDS'))
# mod <- readRDS(here('dataRDS', 'mod.RDS'))
# modSens1 <- readRDS(here('dataRDS', 'modSens1.RDS'))
# modSens2 <- readRDS(here('dataRDS', 'modSens2.RDS'))
# modDfSens3 <- readRDS(here('dataRDS', 'modDfSens3.RDS'))
# modSens3 <- readRDS(here('dataRDS', 'modSens3.RDS'))
# modDf_fracFemur <- readRDS(here('dataRDS', 'modDf_fracFemur.RDS'))
# mod_fracFemur <- readRDS(here('dataRDS', 'mod_fracFemur.RDS'))
# modDf_frailty <- readRDS(here('dataRDS', 'modDf_frailty.RDS'))
# mod_frailty <- readRDS(here('dataRDS', 'mod_frailty.RDS'))


# modUnadjAgeGrpGam <- readRDS(here('dataRDS', 'modUnadjAgeGrpGam.RDS')) 
# modUnadjLoSGrpGam <- readRDS(here('dataRDS', 'modUnadjLoSGrpGam.RDS')) 
# modUnadjCciGrpGam <- readRDS(here('dataRDS', 'modUnadjCciGrpGam.RDS')) 
# modUnadjPriorCommContactAttF2FGrpGam <- readRDS(here('dataRDS', 'modUnadjPriorCommContactAttF2FGrpGam.RDS')) 

modPrelim_Results <- readRDS(here('dataRDS', 'modPrelim_Results.RDS'))
mod_Results <- readRDS(here('dataRDS', 'mod_Results.RDS'))
modUnadj_Results <- readRDS(here('dataRDS', 'modUnadj_Results.RDS'))
sens_Results <- readRDS(here('dataRDS', 'sens_Results.RDS'))
mod_fracFemur_Results <- readRDS(here('dataRDS', 'mod_fracFemur_Results.RDS'))
mod_frailty_Results <- readRDS(here('dataRDS', 'mod_frailty_Results.RDS'))



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


# plot smooth terms
jpeg(here('charts', 'smoothPlotsGrid.jpg'),
     width = 20,
     height = 29,
     units = 'cm',
     res = 150)

par(mfrow = c(4, 2))

#plotAgeUnadj
plot(modUnadjAgeGrpGam, 
     residuals = FALSE, 
     se = TRUE, 
     select = 1,
     trans = exp, 
     ylim = c(0.1, 15), 
     log = 'y',
     ylab = 'unadjusted odds ratio (log scale)',
     xlab = 'age')

#plotAgeAdj
plot(mod, 
     residuals = FALSE, 
     se = TRUE, 
     select = 1, 
     trans = exp, 
     ylim = c(0.1, 15), 
     log = 'y',
     ylab = 'adjusted odds ratio (log scale)',
     xlab = 'age')

#plotLoSUnadj
plot(modUnadjLoSGrpGam, 
                     residuals = FALSE, 
                     se = TRUE, 
                     select = 1,
                     trans = exp, 
                     ylim = c(0.1, 10), 
                     log = 'y',
                     ylab = 'unadjusted odds ratio (log scale)',
                     xlab = 'length of stay (days)')

#plotLoSAdj
plot(mod, 
                     residuals = FALSE, 
                     se = TRUE, 
                     select = 2,
                     trans = exp, 
                     ylim = c(0.1, 10), 
                     log = 'y',
                     ylab = 'adjusted odds ratio (log scale)',
                     xlab = 'length of stay (days)')

#plotCciUnadj
plot(modUnadjCciGrpGam, 
                     residuals = FALSE, 
                     se = TRUE, 
                     select = 1,
                     trans = exp, 
                     ylim = c(0.1, 100), 
                     log = 'y',
                     ylab = 'unadjusted odds ratio (log scale)',
                     xlab = 'Charlson Comorbidity Index')

#plotCciAdj
plot(mod, 
                   residuals = FALSE, 
                   se = TRUE, 
                   select = 3,
                   trans = exp, 
                   ylim = c(0.1, 100), 
                   log = 'y',
                   ylab = 'adjusted odds ratio (log scale)',
                   xlab = 'Charlson Comorbidity Index')

#plotPriorCommUnadj
plot(modUnadjPriorCommContactAttF2FGrpGam, 
                     residuals = FALSE, 
                     se = TRUE, 
                     select = 1,
                     trans = exp, 
                     ylim = c(0.1, 200), 
                     xlim = c(0, 50),
                     log = 'y',
                     ylab = 'unadjusted odds ratio (log scale)',
                     xlab = 'Community contacts 30 days before admission')

#plotPriorCommAdj
plot(mod, 
                   residuals = FALSE, 
                   se = TRUE, 
                   select = 4,
                   trans = exp, 
                   ylim = c(0.1, 200), 
                   xlim = c(0, 50),
                   log = 'y',
                   ylab = 'adjusted odds ratio (log scale)',
                   xlab = 'Community contacts 30 days before admission')

dev.off() 

  





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
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)) +
  scale_y_discrete(name = 'ethnicity', 
                   limits = rev) +
  scale_colour_manual(values = c('grey', 'red')) +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreEthnicityORs.jpg'), 
       device = 'jpg',
       height = 15,
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
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)) +
  scale_y_discrete(name = 'deprivation quintile (IMD2019)', 
                   limits = rev) +
  scale_colour_manual(values = c('grey', 'red')) +
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
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6)) +
  scale_y_discrete(name = 'Integrated Care Board', 
                   limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreIcbORs.jpg'), 
       device = 'jpg',
       height = 16,
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
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.96, 0.98, 1.0)) +
  scale_y_discrete(name = 'sex') +
  scale_colour_manual(values = c('grey', 'red')) +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotCoreSexORs.jpg'), 
       device = 'jpg',
       height = 6,
       width = 15,
       units = 'cm',
       dpi = 300)


# 4 create tables of main model results ----

# tables for mod results
# adj and unadj for deprivation, sex and ethnicity
table2a <- mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth')) %>% 
  select(-statistic) %>% 
  left_join(modUnadj_Results %>% 
              select(term, 
                     estimateUnadj = estimate, 
                     std.errorUnadj = std.error,
                     p.valueUnadj = p.value),
            by = 'term') %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)' ),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3),
                       estimateUnadj = rep(0, 3),
                       std.errorUnadj = rep(NA_real_, 3),
                       p.valueUnadj = rep(NA_real_, 3))) %>%  
  mutate(or_adj = exp(estimate),
         lcl95_adj = exp(estimate - 1.96*std.error),
         ucl95_adj = exp(estimate + 1.96*std.error),
         or_unadj = exp(estimateUnadj),
         lcl95_unadj = exp(estimateUnadj - 1.96*std.errorUnadj),
         ucl95_unadj = exp(estimateUnadj + 1.96*std.errorUnadj)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               TRUE ~ 'check')) %>% 
  mutate(termLabel = factor(termLabel, 
                            levels = c('1 - most deprived (ref)', '2', '3', '4', '5',
                                       'male (ref)', 'female',
                                       'White - British (ref)', 'White - Irish', 'White - other groups', 'Asian - Indian', 'Asian - Pakistani',
                                       'Asian - Bangladeshi', 'Asian - other groups', 'Black - Caribbean', 'Black - African', 'Black - other groups',
                                       'Mixed - White & Black Caribbean', 'Mixed - White & Black African', 'Mixed - White & Asian', 'Mixed - other groups', 'Chinese',
                                       'Other groups', 'Not stated / known'))) %>% 
  select(termGrp, term, termLabel, 
         or_unadj, lcl95_unadj, ucl95_unadj, p.valueUnadj, 
         or_adj, lcl95_adj, ucl95_adj, p.value) %>% 
  arrange(termGrp, termLabel)


write.csv(table2a, here('tables', 'table2a.csv'))


# adj and unadj for icb
table2b <- mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('icb')) %>% 
  select(-statistic) %>% 
  left_join(modUnadj_Results %>% 
              select(term, 
                     estimateUnadj = estimate, 
                     std.errorUnadj = std.error,
                     p.valueUnadj = p.value),
            by = 'term') %>% 
  bind_rows(data.frame(term = c('icbShortNameCambridgeshire & Peterborough (ref)'),
                       estimate = rep(0, 1),
                       std.error = rep(NA_real_, 1),
                       p.value = rep(NA_real_, 1),
                       estimateUnadj = rep(0, 1),
                       std.errorUnadj = rep(NA_real_, 1),
                       p.valueUnadj = rep(NA_real_, 1))) %>%  
  mutate(or_adj = exp(estimate),
         lcl95_adj = exp(estimate - 1.96*std.error),
         ucl95_adj = exp(estimate + 1.96*std.error),
         or_unadj = exp(estimateUnadj),
         lcl95_unadj = exp(estimateUnadj - 1.96*std.errorUnadj),
         ucl95_unadj = exp(estimateUnadj + 1.96*std.errorUnadj)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'icb' ~ 'ICB',
                             TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('ICB'))) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'icb' ~ substr(term, 13, nchar(term)),
                               TRUE ~ 'check')) %>% 
  mutate(termLabel = factor(termLabel, 
                            levels = c('Cambridgeshire & Peterborough (ref)', 'Bath, NE Somerset, Swindon & Wiltshire', 'Bedfordshire, Luton & Milton Keynes', 'Birmingham & Solihul', 'Black Country',
                                       'Bristol, N Somerset, Swindon & S Gloucestershire', 'Buckinghamshire, Oxfordshire &  W Berkshire', 'Cheshire & Merseyside', 'Cornwall & Isles of Scilly', 'Coventry & Warwickshire',
                                       'Derby & Derbyshire', 'Devon', 'Frimley', 'Gloucestershire', 'Greater Manchester',
                                       'Hampshire & Isle of Wight', 'Herefordshire & Worcestershire', 'Hertfordshire & W Essex', 'Humber & N Yorkshire', 'Kent & Medway',
                                       'Lancashire & S Cumbria', 'Leicester, Leicestershire & Rutland', 'Lincolnshire', 'Mid & S Essex', 'N Central London',
                                       'NE & N Cumbria', 'NE London', 'Norfolk & Waveney', 'Northamptonshire', 'Nottingham & Nottinghamshire',
                                       'NW London', 'SE London', 'Shropshire, Telford & Wrekin', 'Somerset', 'South Yorkshire',
                                       'Staffordshire & Stoke-on-Trent', 'Suffolk & NE Essex', 'Surrey Heartlands', 'Sussex', 'SW London', 
                                       'W Yorkshire'))) %>% 
  select(termGrp, term, termLabel, 
         or_unadj, lcl95_unadj, ucl95_unadj, p.valueUnadj, 
         or_adj, lcl95_adj, ucl95_adj, p.value) %>% 
  arrange(termGrp, termLabel)

write.csv(table2b, here('tables', 'table2b.csv'))



# adj and unadj for pod
table2c <- mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('pod')) %>% 
  select(-statistic) %>% 
  left_join(modUnadj_Results %>% 
              select(term, 
                     estimateUnadj = estimate, 
                     std.errorUnadj = std.error,
                     p.valueUnadj = p.value),
            by = 'term') %>% 
  bind_rows(data.frame(term = c('podGrpnon-elective (ref)'),
                       estimate = rep(0, 1),
                       std.error = rep(NA_real_, 1),
                       p.value = rep(NA_real_, 1),
                       estimateUnadj = rep(0, 1),
                       std.errorUnadj = rep(NA_real_, 1),
                       p.valueUnadj = rep(NA_real_, 1))) %>%  
  mutate(or_adj = exp(estimate),
         lcl95_adj = exp(estimate - 1.96*std.error),
         ucl95_adj = exp(estimate + 1.96*std.error),
         or_unadj = exp(estimateUnadj),
         lcl95_unadj = exp(estimateUnadj - 1.96*std.errorUnadj),
         ucl95_unadj = exp(estimateUnadj + 1.96*std.errorUnadj)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'pod' ~ 'Admission method',
                             TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('Admission method'))) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'pod' ~ substr(term, 7, nchar(term)),
                               TRUE ~ 'check')) %>% 
  mutate(termLabel = factor(termLabel, 
                            levels = c('non-elective (ref)', 'elective'))) %>% 
  select(termGrp, term, termLabel, 
         or_unadj, lcl95_unadj, ucl95_unadj, p.valueUnadj, 
         or_adj, lcl95_adj, ucl95_adj, p.value) %>% 
  arrange(termGrp, termLabel)

write.csv(table2c, here('tables', 'table2c.csv'))



# adj and unadj for specialty
tretSpefLookup <- read.csv(here('dataRaw', 'tretSpefLookup.csv'), header = FALSE) %>% 
  select(specCode = V1,
         specName = V2) %>% 
  bind_rows(data.frame(specCode = 'otherSpec',
                       specName = 'all other specialties'))

table2d <- mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('spe')) %>% 
  select(-statistic) %>% 
  left_join(modUnadj_Results %>% 
              select(term, 
                     estimateUnadj = estimate, 
                     std.errorUnadj = std.error,
                     p.valueUnadj = p.value),
            by = 'term') %>% 
  bind_rows(data.frame(term = c('specGrp300'),
                       estimate = rep(0, 1),
                       std.error = rep(NA_real_, 1),
                       p.value = rep(NA_real_, 1),
                       estimateUnadj = rep(0, 1),
                       std.errorUnadj = rep(NA_real_, 1),
                       p.valueUnadj = rep(NA_real_, 1))) %>%  
  mutate(or_adj = exp(estimate),
         lcl95_adj = exp(estimate - 1.96*std.error),
         ucl95_adj = exp(estimate + 1.96*std.error),
         or_unadj = exp(estimateUnadj),
         lcl95_unadj = exp(estimateUnadj - 1.96*std.errorUnadj),
         ucl95_unadj = exp(estimateUnadj + 1.96*std.errorUnadj)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 7) == 'specGrp' ~ 'Specialty',
                             TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('Specialty'))) %>% 
  mutate(specCode = substr(term, 8, nchar(term))) %>% 
  left_join(tretSpefLookup, by = c('specCode')) %>%        
  mutate(termLabel = specName) %>% 
  mutate(termLabel = ifelse(termLabel == 'General Medicine', 'General Medicine (ref)', termLabel)) %>% 
  mutate(termLabel = factor(termLabel)) %>% 
  mutate(termLabel = relevel(termLabel, ref = 'General Medicine (ref)')) %>% 
  select(termGrp, term, termLabel, 
         or_unadj, lcl95_unadj, ucl95_unadj, p.valueUnadj, 
         or_adj, lcl95_adj, ucl95_adj, p.value) %>% 
  arrange(termGrp, termLabel)

write.csv(table2d, here('tables', 'table2d.csv'))

# 5 create chart outputs for sensitivity analysis----
sens_Results %>% 
  mutate(lcl95 = ifelse(mod == 'mod', lcl95, NA_real_),
         ucl95 = ifelse(mod == 'mod', ucl95, NA_real_),
         or = ifelse(mod != 'mod' & termLabel == '1 - most deprived (ref)', NA_real_, or)) %>% 
  mutate(modLabel = case_when(mod == 'mod' ~ 'main analysis',
                              mod == 'modSens1' ~ 'sensitiviy analysis 1',
                              mod == 'modSens2' ~ 'sensitiviy analysis 2',
                              mod == 'modSens3' ~ 'sensitiviy analysis 3')) %>% 
  mutate(adjYPos = case_when(mod == 'mod' ~ 0,
                              mod == 'modSens1' ~ 0.1,
                              mod == 'modSens2' ~ 0.1,
                              mod == 'modSens3' ~ 0.1)) %>% 
  mutate(termLabel2 = case_when(termLabel == '1 - most deprived (ref)' ~ 1 + adjYPos,
                                termLabel == '2' ~ 2 + adjYPos,
                                termLabel == '3' ~ 3 + adjYPos,
                                termLabel == '4' ~ 4 + adjYPos,
                                termLabel == '5' ~ 5 + adjYPos)) %>% 
  filter(termGrp == 'IMD') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel2, yend = termLabel2, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel2, x = or, colour = modLabel, size = modLabel), )  +
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.85, 0.9, 0.95, 1.0)) +
  scale_y_reverse(name = 'deprivation quintile (IMD2019)',
                  breaks = c(1, 2, 3, 4, 5), 
                  labels = c('1 - most deprived (ref)', '2', '3', '4', '5')) +
  scale_colour_manual(values = c('gray', 'orange', 'purple', 'blue'))  +
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
  mutate(lcl95 = ifelse(mod == 'mod', lcl95, NA_real_),
         ucl95 = ifelse(mod == 'mod', ucl95, NA_real_),
         or = ifelse(mod != 'mod' & termLabel == 'White - British (ref)', NA_real_, or)) %>% 
  mutate(modLabel = case_when(mod == 'mod' ~ 'main analysis',
                              mod == 'modSens1' ~ 'sensitiviy analysis 1',
                              mod == 'modSens2' ~ 'sensitiviy analysis 2',
                              mod == 'modSens3' ~ 'sensitiviy analysis 3')) %>% 
  mutate(adjYPos = case_when(mod == 'mod' ~ 0,
                             mod == 'modSens1' ~ 0.1,
                             mod == 'modSens2' ~ 0.1,
                             mod == 'modSens3' ~ 0.1)) %>% 
  mutate(termLabel2 = case_when(termLabel == 'White - British (ref)' ~ 1 + adjYPos,
                                termLabel == 'White - Irish' ~ 2 + adjYPos,
                                termLabel == 'White - other groups' ~ 3 + adjYPos,
                                termLabel == 'Asian - Indian' ~ 4 + adjYPos,
                                termLabel == 'Asian - Pakistani' ~ 5 + adjYPos,
                                termLabel == 'Asian - Bangladeshi' ~ 6 + adjYPos,
                                termLabel == 'Asian - other groups' ~ 7 + adjYPos,
                                termLabel == 'Black - Caribbean' ~ 8 + adjYPos,
                                termLabel == 'Black - African' ~ 9 + adjYPos,
                                termLabel == 'Black - other groups' ~ 10 + adjYPos,
                                termLabel == 'Mixed - White & Black Caribbean' ~ 11 + adjYPos,
                                termLabel == 'Mixed - White & Black African' ~ 12 + adjYPos,
                                termLabel == 'Mixed - White & Asian' ~ 13 + adjYPos,
                                termLabel == 'Mixed - other groups' ~ 14 + adjYPos,
                                termLabel == 'Chinese' ~ 15 + adjYPos,
                                termLabel == 'Other groups' ~ 16 + adjYPos,
                                termLabel == 'Not stated / known' ~ 17 + adjYPos)) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel2, yend = termLabel2, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel2, x = or, colour = modLabel, size = modLabel)) +
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0, 1.1, 1.2)) +
  scale_y_reverse(name = 'ethnicity',
                  breaks = seq(1, 17, 1), 
                  labels = c('White - British (ref)', 'White - Irish', 'White - other groups', 
                             'Asian - Indian', 'Asian - Pakistani', 'Asian - Bangladeshi',
                             'Asian - other groups', 'Black - Caribbean', 'Black - African',
                             'Black - other groups', 'Mixed - White & Black Caribbean', 
                             'Mixed - White & Black African', 'Mixed - White & Asian', 'Mixed - other groups',
                             'Chinese', 'Other groups', 'Not stated / known')) +
  scale_colour_manual(values = c('grey', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

ggsave(here('charts', 'plotSensEthnicityORs.jpg'), 
       device = 'jpg',
       height = 16,
       width = 15,
       units = 'cm',
       dpi = 300)

sens_Results %>% 
  filter(termGrp == 'Sex') %>% 
  mutate(lcl95 = ifelse(mod == 'mod', lcl95, NA_real_),
         ucl95 = ifelse(mod == 'mod', ucl95, NA_real_),
         or = ifelse(mod != 'mod' & termLabel == 'male (ref)', NA_real_, or)) %>% 
  mutate(modLabel = case_when(mod == 'mod' ~ 'main analysis',
                              mod == 'modSens1' ~ 'sensitiviy analysis 1',
                              mod == 'modSens2' ~ 'sensitiviy analysis 2',
                              mod == 'modSens3' ~ 'sensitiviy analysis 3')) %>% 
  mutate(adjYPos = case_when(mod == 'mod' ~ 0,
                             mod == 'modSens1' ~ 0.1,
                             mod == 'modSens2' ~ 0.1,
                             mod == 'modSens3' ~ 0.1)) %>% 
  mutate(termLabel2 = case_when(termLabel == 'male (ref)' ~ 1 + adjYPos,
                                termLabel == 'female' ~ 2 + adjYPos)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel2, yend = termLabel2, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel2, x = or, colour = modLabel, size = modLabel)) +
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.96, 0.98, 1.0)) +
  scale_y_reverse(name = 'sex',
                    breaks = seq(1, 2, 1), 
                    labels = c('male (ref)', 'female')) +
  scale_colour_manual(values = c('grey', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

ggsave(here('charts', 'plotSensSexORs.jpg'), 
       device = 'jpg',
       height = 4,
       width = 15,
       units = 'cm',
       dpi = 300)

sens_Results %>% 
  filter(termGrp == 'ICB') %>% 
  mutate(lcl95 = ifelse(mod == 'mod', lcl95, NA_real_),
         ucl95 = ifelse(mod == 'mod', ucl95, NA_real_),
         or = ifelse(mod != 'mod' & termLabel == 'Cambridgeshire & Peterborough (ref)', NA_real_, or)) %>% 
  mutate(modLabel = case_when(mod == 'mod' ~ 'main analysis',
                              mod == 'modSens1' ~ 'sensitiviy analysis 1',
                              mod == 'modSens2' ~ 'sensitiviy analysis 2',
                              mod == 'modSens3' ~ 'sensitiviy analysis 3')) %>% 
  mutate(adjYPos = case_when(mod == 'mod' ~ 0,
                             mod == 'modSens1' ~ 0.1,
                             mod == 'modSens2' ~ 0.1,
                             mod == 'modSens3' ~ 0.1)) %>% 
  mutate(termLabel2 = case_when(termLabel == 'Cambridgeshire & Peterborough (ref)'        ~ 1 + adjYPos,
                                termLabel == 'Bath, NE Somerset, Swindon & Wiltshire'     ~ 2 + adjYPos,
                                termLabel == 'Bedfordshire, Luton & Milton Keynes'        ~ 3 + adjYPos,
                                termLabel == 'Birmingham & Solihul'                       ~ 4 + adjYPos,
                                termLabel == 'Black Country'                              ~ 5 + adjYPos,
                                termLabel == 'Bristol, N Somerset, Swindon & S Gloucs'    ~ 6 + adjYPos,
                                termLabel == 'Buckinghamshire, Oxfordshire & W Berkshire' ~ 7 + adjYPos,
                                termLabel == 'Cheshire & Merseyside'                      ~ 8 + adjYPos,
                                termLabel == 'Cornwall & Isles of Scilly'                 ~ 9 + adjYPos,
                                termLabel == 'Coventry & Warwickshire'                    ~ 10 + adjYPos,
                                termLabel == 'Derby & Derbyshire'                         ~ 11 + adjYPos,
                                termLabel == 'Devon'                                      ~ 12 + adjYPos,
                                termLabel == 'Frimley'                                    ~ 13 + adjYPos,
                                termLabel == 'Gloucestershire'                            ~ 14 + adjYPos,
                                termLabel == 'Greater Manchester'                         ~ 15 + adjYPos,
                                termLabel == 'Hampshire & Isle of Wight'                  ~ 16 + adjYPos,
                                termLabel == 'Herefordshire & Worcestershire'             ~ 17 + adjYPos,
                                termLabel == 'Hertfordshire & W Essex'                    ~ 18 + adjYPos,
                                termLabel == 'Humber & N Yorkshire'                       ~ 19 + adjYPos,
                                termLabel == 'Kent & Medway'                              ~ 20 + adjYPos,
                                termLabel == 'Lancashire & S Cumbria'                     ~ 21 + adjYPos,
                                termLabel == 'Leicester, Leicestershire & Rutland'        ~ 22 + adjYPos,
                                termLabel == 'Lincolnshire'                               ~ 23 + adjYPos,
                                termLabel == 'Mid & S Essex'                              ~ 24 + adjYPos,
                                termLabel == 'N Central London'                           ~ 25 + adjYPos,
                                termLabel == 'NE & N Cumbria'                             ~ 26 + adjYPos,
                                termLabel == 'NE London'                                  ~ 27 + adjYPos,
                                termLabel == 'Norfolk & Waveney'                          ~ 28 + adjYPos,
                                termLabel == 'Northamptonshire'                           ~ 29 + adjYPos,
                                termLabel == 'Nottingham & Nottinghamshire'               ~ 30 + adjYPos,
                                termLabel == 'NW London'                                  ~ 31 + adjYPos,
                                termLabel == 'SE London'                                  ~ 32 + adjYPos,
                                termLabel == 'Shropshire, Telford & Wrekin'               ~ 33 + adjYPos,
                                termLabel == 'Somerset'                                   ~ 34 + adjYPos,
                                termLabel == 'South Yorkshire'                            ~ 35 + adjYPos,
                                termLabel == 'Staffordshire & Stoke-on-Trent'             ~ 36 + adjYPos,
                                termLabel == 'Suffolk & NE Essex'                         ~ 37 + adjYPos,
                                termLabel == 'Surrey Heartlands'                          ~ 38 + adjYPos,
                                termLabel == 'Sussex'                                     ~ 39 + adjYPos,
                                termLabel == 'SW London'                                  ~ 40 + adjYPos,
                                termLabel == 'W Yorkshire'                                ~ 41 + adjYPos,)) %>%
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel2, yend = termLabel2, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel2, x = or, colour = modLabel, size = modLabel)) +
  scale_x_log10(name ='adjusted odds ratio (log scale)',
                breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4)) +
  scale_y_reverse(name = 'Integrated Care Board',
                  breaks = seq(1, 41, 1), 
                  labels = c('Cambridgeshire & Peterborough (ref)', 'Bath, NE Somerset, Swindon & Wiltshire', 
                             'Bedfordshire, Luton & Milton Keynes', 'Birmingham & Solihull', 'Black Country', 
                             'Bristol, N Somerset, Swindon & S Gloucs', 'Buckinghamshire, Oxfordshire & W Berkshire', 
                             'Cheshire & Merseyside' ,'Cornwall & Isles of Scilly', 'Coventry & Warwickshire', 
                             'Derby & Derbyshire' ,'Devon', 'Frimley' ,'Gloucestershire' ,'Greater Manchester',
                             'Hampshire & Isle of Wight',' Herefordshire & Worcestershire', 'Hertfordshire & W Essex', 
                             'Humber & N Yorkshire', 'Kent & Medway', 'Lancashire & S Cumbria', 
                             'Leicester, Leicestershire & Rutland', 'Lincolnshire', 'Mid & S Essex', 
                             'N Central London', 'NE & N Cumbria', 'NE London', 'Norfolk & Waveney', 
                             'Northamptonshire', 'Nottingham & Nottinghamshire', 'NW London', 'SE London' , 
                             'Shropshire, Telford & Wrekin', 'Somerset', 'South Yorkshire' ,
                             'Staffordshire & Stoke-on-Trent', 'Suffolk & NE Essex', 'Surrey Heartlands',
                             'Sussex', 'SW London', 'W Yorkshire')) +
  scale_colour_manual(values = c('grey', 'orange', 'purple', 'blue'))  +
  scale_size_manual(values = c(3, 1, 1, 1)) +
  theme(legend.title = element_blank())

                          
ggsave(here('charts', 'plotSensICBORs.jpg'), 
       device = 'jpg',
       height = 17,
       width = 15,
       units = 'cm',
       dpi = 300)


# 6 create charts for subgroup analysis----
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
  mutate(termLabel = factor(termLabel, levels = c('1 - most deprived (ref)', '2', '3', '4', '5',
                                                  'White (ref)', 'Asian', 'Black', 'Mixed heritage', 'Other groups', 'Not stated / known', 
                                                  'male (ref)', 'female'))) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +  
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  scale_x_log10(name ='adjusted odds ratio (log scale)') +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

ggsave(here('charts', 'plotFracFemurORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 20,
       units = 'cm',
       dpi = 300)


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
  mutate(termLabel = factor(termLabel, levels = c('1 - most deprived (ref)', '2', '3', '4', '5',
                                                  'White (ref)', 'Asian', 'Black', 'Mixed heritage', 'Other groups', 'Not stated / known', 
                                                  'male (ref)', 'female'))) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1')) %>%
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_point(aes(y = termLabel, x = or, colour = orSigGrp)) +  
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  scale_x_log10(name ='adjusted odds ratio (log scale)') +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')


ggsave(here('charts', 'plotFrailtyORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 20,
       units = 'cm',
       dpi = 300)

# 7 create tables for subgroup analysis ----
table4a <- mod_fracFemur_Results %>% 
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
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) %>% 
  mutate(termLabel = factor(termLabel, levels = c('1 - most deprived (ref)', '2', '3', '4', '5',
                                                  'White (ref)', 'Asian', 'Black', 'Mixed heritage', 'Other groups', 'Not stated / known', 
                                                  'male (ref)', 'female'))) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1'))  %>% 
  select(termGrp, termLabel, or, lcl95, ucl95, p.value) %>% 
  arrange(termGrp, termLabel)


write.csv(table4a, here('tables', 'table4a.csv'))



table4b <- mod_frailty_Results %>% 
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
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) %>% 
  mutate(termLabel = factor(termLabel, levels = c('1 - most deprived (ref)', '2', '3', '4', '5',
                                                  'White (ref)', 'Asian', 'Black', 'Mixed heritage', 'Other groups', 'Not stated / known', 
                                                  'male (ref)', 'female'))) %>% 
  mutate(orSigGrp = case_when(lcl95 > 1 ~ 'sigHigher',
                              ucl95 < 1 ~ 'sigLower',
                              TRUE ~ 'notSigDiff1'))  %>% 
  select(termGrp, termLabel, or, lcl95, ucl95, p.value) %>% 
  arrange(termGrp, termLabel)


write.csv(table4b, here('tables', 'table4b.csv'))
