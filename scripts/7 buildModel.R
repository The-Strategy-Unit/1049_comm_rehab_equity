# this script builds models to explore equity of access to community services after discharge from hospital
# this script needs to be run in the NCDR data science environment

# 0 set_up ----

library(tidyverse)
library(here)
library(broom)
library(mgcv)


# (A) models for NHSE paper ----

# 1 load model data frame ----
modDf <- readRDS(here('dataRDS', 'modDf.RDS'))


# 2 build unadjusted (univariate) models ----
modUnadjImdQF <- glm(postCommContactAttF2FYN ~ imdQF,
                 data = modDf,
                 family = binomial(link = "logit"))

modUnadjSexMf <- glm(postCommContactAttF2FYN ~ sexMf,
               data = modDf,
               family = binomial(link = "logit"))

modUnadjEthnicity <- glm(postCommContactAttF2FYN ~ ethnicity,
               data = modDf,
               family = binomial(link = "logit"))

modUnadjIcbShortName <- glm(postCommContactAttF2FYN ~ icbShortName,
                   data = modDf,
                   family = binomial(link = "logit"))

modUnadjAgeGrp <- glm(postCommContactAttF2FYN ~ ageGrp,
                      data = modDf,
                      family = binomial(link = "logit"))

modUnadjLoSGrp <- glm(postCommContactAttF2FYN ~ loSGrp,
                   data = modDf,
                   family = binomial(link = "logit"))

modUnadjCciGrp <- glm(postCommContactAttF2FYN ~ cciGrp,
                   data = modDf,
                   family = binomial(link = "logit"))

modUnadjSpecGrp <- glm(postCommContactAttF2FYN ~ specGrp,
                data = modDf,
                family = binomial(link = "logit"))

modUnadjPodGrp <- glm(postCommContactAttF2FYN ~ podGrp,
                data = modDf,
                family = binomial(link = "logit"))

modUnadjPriorCommContactAttF2FGrp <- glm(postCommContactAttF2FYN ~ priorCommContactAttF2FGrp,
                data = modDf,
                family = binomial(link = "logit"))

saveRDS(modUnadjImdQF, here('dataRDS', 'modUnadjImdQF.RDS'))
saveRDS(modUnadjSexMf, here('dataRDS', 'modUnadjSexMf.RDS'))
saveRDS(modUnadjEthnicity, here('dataRDS', 'modUnadjEthnicity.RDS'))
saveRDS(modUnadjIcbShortName, here('dataRDS', 'modUnadjIcbShortName.RDS'))
saveRDS(modUnadjAgeGrp, here('dataRDS', 'modUnadjAgeGrp.RDS'))
saveRDS(modUnadjLoSGrp, here('dataRDS', 'modUnadjLoSGrp.RDS'))
saveRDS(modUnadjCciGrp, here('dataRDS', 'modUnadjCciGrp.RDS'))
saveRDS(modUnadjSpecGrp, here('dataRDS', 'modUnadjSpecGrp.RDS'))
saveRDS(modUnadjPodGrp, here('dataRDS', 'modUnadjPodGrp.RDS'))
saveRDS(modUnadjPriorCommContactAttF2FGrp, here('dataRDS', 'modUnadjPriorCommContactAttF2FGrp.RDS'))

# now with gams for age, los cci, prior comm contacts

modUnadjAgeGrpGam <- bam(postCommContactAttF2FYN ~ s(Der_Age_at_CDS_Activity_Date, bs = 'cr'),
                        data = modDf,
                        family = binomial(link = "logit"))

modUnadjLoSGrpGam <- bam(postCommContactAttF2FYN ~ s(der_spell_LoS, bs = 'cr'),
                        data = modDf,
                        family = binomial(link = "logit"))

modUnadjCciGrpGam <- bam(postCommContactAttF2FYN ~ s(cci, bs = 'cr'),
                       data = modDf,
                       family = binomial(link = "logit"))

modUnadjPriorCommContactAttF2FGrpGam <- bam(postCommContactAttF2FYN ~ s(priorCommContactsAttF2F, bs = 'cr'),
                                          data = modDf,
                                          family = binomial(link = "logit"))

saveRDS(modUnadjAgeGrpGam, here('dataRDS', 'modUnadjAgeGrpGam.RDS'))
saveRDS(modUnadjLoSGrpGam, here('dataRDS', 'modUnadjLoSGrpGam.RDS'))
saveRDS(modUnadjCciGrpGam, here('dataRDS', 'modUnadjCciGrpGam.RDS'))
saveRDS(modUnadjPriorCommContactAttF2FGrpGam, here('dataRDS', 'modUnadjPriorCommContactAttF2FGrpGam.RDS'))

modUnadj_Results <-  
  tidy(modUnadjImdQF, exponentiate = FALSE) %>% 
  bind_rows(tidy(modUnadjSexMf, exponentiate = FALSE)) %>% 
  bind_rows(tidy(modUnadjEthnicity, exponentiate = FALSE)) %>% 
  bind_rows(tidy(modUnadjIcbShortName, exponentiate = FALSE)) %>% 
  bind_rows(tidy(modUnadjSpecGrp, exponentiate = FALSE)) %>% 
  bind_rows(tidy(modUnadjPodGrp, exponentiate = FALSE)) %>% 
  filter(term != '(Intercept)')

saveRDS(modUnadj_Results, here('dataRDS', 'modUnadj_Results.RDS'))



# 3 build preliminary adjusted (multivariable) model - using grouped variables ----  
modPrelim <- glm(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             ageGrp + loSGrp + cciGrp + specGrp + podGrp + priorCommContactAttF2FGrp,
           data = modDf,
           family = binomial(link = "logit"))

saveRDS(modPrelim, here('dataRDS', 'modPrelim.RDS'))

modPrelim_Results <- tidy(modPrelim, exponentiate = FALSE) 

saveRDS(modPrelim_Results, here('dataRDS', 'modPrelim_Results.RDS'))



# 4 build final adjusted (multivariable) model with gam terms for age, los, cci, prior comm contacts ----
# bam used in place of gam because dataset large
mod <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDf,
           family = binomial(link = "logit"))

saveRDS(mod, here('dataRDS', 'mod.RDS'))

mod_Results <- tidy(mod, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_Results, here('dataRDS', 'mod_Results.RDS'))


# 5 sensitivity analyses ----
# sens1 - all atts (not just f2f)
modSens1 <- bam(postCommContactAttYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAtt, bs = 'cr'),
           data = modDf,
           family = binomial(link = "logit"))

saveRDS(modSens1, here('dataRDS', 'modSens1.RDS'))


# sens2 - all apps whether attended or not
modSens2 <- bam(postCommContactAppYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
                  specGrp + podGrp + 
                  s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                  s(der_spell_LoS, bs = 'cr') + 
                  s(cci, bs = 'cr') + 
                  s(priorCommContactsApp, bs = 'cr'),
                data = modDf,
                family = binomial(link = "logit"))

saveRDS(modSens2, here('dataRDS', 'modSens2.RDS'))

# sens3 - excluding iCBs with data issues
modDfSens3 <- modDf %>% 
  filter(STP21CD != 'E54000048') %>% # Lancs and SC
  filter(STP21CD != 'E54000051') %>% # Humber and NY
  filter(STP21CD != 'E54000009') %>% # S Yorks
  filter(STP21CD != 'E54000052') %>% # Surrey H
  filter(STP21CD != 'E54000019') %>% # Heref & Worces
  filter(STP21CD != 'E54000025') %>% # Herts and W Essex 
  filter(STP21CD != 'E54000054') %>% # W Yorks
  filter(STP21CD != 'E54000053') %>% # Sussex
  filter(STP21CD != 'E54000039') %>% # Bristol, NS & SG
  filter(STP21CD != 'E54000037')     # Devon
 
modDfSens3 %>% 
  summarise(n = n())
 

modSens3 <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDfSens3,
           family = binomial(link = "logit"))

saveRDS(modDfSens3, here('dataRDS', 'modDfSens3.RDS'))
saveRDS(modSens3, here('dataRDS', 'modSens3.RDS'))

## 

mod_Results <- tidy(mod, exponentiate = FALSE, parametric = TRUE) 
modSens1_Results <- tidy(modSens1, exponentiate = FALSE, parametric = TRUE) 
modSens2_Results <- tidy(modSens2, exponentiate = FALSE, parametric = TRUE) 
modSens3_Results <- tidy(modSens3, exponentiate = FALSE, parametric = TRUE) 


sens_Results <- mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(0, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(mod = 'mod') %>% 
  bind_rows(modSens1_Results %>% 
              filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
              bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                                   estimate = rep(0, 4),
                                   std.error = rep(NA_real_, 4),
                                   statistics = rep(NA_real_, 4),
                                   p.value = rep(NA_real_, 4))) %>% 
              mutate(mod = 'modSens1')) %>% 
  bind_rows(modSens2_Results %>% 
              filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
              bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                                   estimate = rep(0, 4),
                                   std.error = rep(NA_real_, 4),
                                   statistics = rep(NA_real_, 4),
                                   p.value = rep(NA_real_, 4))) %>% 
              mutate(mod = 'modSens2')) %>% 
  bind_rows(modSens3_Results %>% 
              filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
              bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                                   estimate = rep(0, 4),
                                   std.error = rep(NA_real_, 4),
                                   statistics = rep(NA_real_, 4),
                                   p.value = rep(NA_real_, 4))) %>% 
              mutate(mod = 'modSens3')) %>%  
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
                               TRUE ~ 'check'))

saveRDS(sens_Results, here('dataRDS', 'sens_Results.RDS'))

# 6 subgroup analysis----
# fractured femur

modDf_fracFemur <- modDf %>% 
  filter(grepl('S72', der_diagnosis_all)) %>% 
  filter(podGrp == 'nonElective')

modDf_fracFemur %>% 
  summarise(n = n(),
            p = n() / 938581)

modDf_fracFemur %>% 
  group_by(postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n))

saveRDS(modDf_fracFemur, here('dataRDS', 'modDf_fracFemur.RDS'))

mod_fracFemur <- gam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicityGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDf_fracFemur,
           family = binomial(link = "logit"))



saveRDS(mod_fracFemur, here('dataRDS', 'mod_fracFemur.RDS'))

mod_fracFemur_Results <- tidy(mod_fracFemur, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_fracFemur_Results, here('dataRDS', 'mod_fracFemur_Results.RDS'))



# frailty
# define as frailty score >  15, using method described by 
# Gilbert et al, 2018 (https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)30668-8/fulltext)



modDf_frailty <- modDf %>% 
  filter(podGrp == 'nonElective') %>% 
  filter(Der_Age_at_CDS_Activity_Date >= 75) %>% 
  mutate(frailtyScore = ifelse(grepl('F00', der_diagnosis_all), 7.1, 0) +
           ifelse(grepl('G81', der_diagnosis_all), 4.4, 0) +
           ifelse(grepl('G30', der_diagnosis_all), 4.0, 0) +
           ifelse(grepl('I69', der_diagnosis_all), 3.7, 0) +
           ifelse(grepl('R29', der_diagnosis_all), 3.6, 0) +
           ifelse(grepl('N39', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('F05', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('W19', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('S00', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('R31', der_diagnosis_all), 3.0, 0) +
           ifelse(grepl('B86', der_diagnosis_all), 2.9, 0) +
           ifelse(grepl('R41', der_diagnosis_all), 2.7, 0) +
           ifelse(grepl('R26', der_diagnosis_all), 2.6, 0) +
           ifelse(grepl('I67', der_diagnosis_all), 2.6, 0) +
           ifelse(grepl('R56', der_diagnosis_all), 2.6, 0) +
           ifelse(grepl('R40', der_diagnosis_all), 2.5, 0) +
           ifelse(grepl('T83', der_diagnosis_all), 2.4, 0) +
           ifelse(grepl('S06', der_diagnosis_all), 2.4, 0) +
           
           ifelse(grepl('S42', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('E87', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('M25', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('E86', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('R54', der_diagnosis_all), 2.2, 0) +
           ifelse(grepl('Z50', der_diagnosis_all), 2.1, 0) +
           ifelse(grepl('F03', der_diagnosis_all), 2.1, 0) +
           ifelse(grepl('W18', der_diagnosis_all), 2.1, 0) +
           ifelse(grepl('Z75', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('F01', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('S80', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('L03', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('H54', der_diagnosis_all), 1.9, 0) +
           ifelse(grepl('E53', der_diagnosis_all), 1.9, 0) +
           ifelse(grepl('Z60', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('G20', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('R55', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('S22', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('K59', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('N17', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('L89', der_diagnosis_all), 1.7, 0) +
           ifelse(grepl('Z22', der_diagnosis_all), 1.7, 0) +
           ifelse(grepl('B95', der_diagnosis_all), 1.7, 0) +
           ifelse(grepl('L97', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('R44', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('K26', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('I95', der_diagnosis_all), 1.6, 0) +
           
           ifelse(grepl('N19', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('A41', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('Z87', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('J96', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('X59', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('M19', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('G40', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('M81', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('S72', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('S32', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('E16', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('R94', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('N18', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('R33', der_diagnosis_all), 1.3, 0) +
           ifelse(grepl('R69', der_diagnosis_all), 1.3, 0) +
           ifelse(grepl('N28', der_diagnosis_all), 1.3, 0) +
           ifelse(grepl('R32', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('G31', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('Y95', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('S09', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('R45', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('G45', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('Z74', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('M79', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('W06', der_diagnosis_all), 1.1, 0) +
           
           ifelse(grepl('S01', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('A04', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('A09', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('J18', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('J69', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('R47', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('E55', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('Z93', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('R02', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('R63', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('H91', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('W10', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('W01', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('E05', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('M41', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('R13', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('Z99', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('U80', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('M80', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('K92', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('I63', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('N20', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('F10', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('Y84', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('R00', der_diagnosis_all), 0.7, 0) +
           
           ifelse(grepl('J22', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('Z73', der_diagnosis_all), 0.6, 0) +
           ifelse(grepl('R79', der_diagnosis_all), 0.6, 0) +
           ifelse(grepl('Z91', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('S51', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('F32', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('M48', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('E83', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('M15', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('D64', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('L08', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('R11', der_diagnosis_all), 0.3, 0) +
           ifelse(grepl('K52', der_diagnosis_all), 0.3, 0) +
           ifelse(grepl('R50', der_diagnosis_all), 0.1, 0)) %>% 
  filter(frailtyScore > 15)


saveRDS(modDf_frailty, here('dataRDS', 'modDf_frailty.RDS'))

modDf_frailty %>% 
  summarise(n = n(),
            p = n() / 938581)

modDf_frailty %>% 
  group_by(postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n))

mod_frailty <- gam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicityGrp + 
                       s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                       s(der_spell_LoS, bs = 'cr') + 
                       s(cci, bs = 'cr') + 
                       s(priorCommContactsAttF2F, bs = 'cr'),
                     data = modDf_frailty,
                     family = binomial(link = "logit"))

saveRDS(mod_frailty, here('dataRDS', 'mod_frailty.RDS'))

mod_frailty_Results <- tidy(mod_frailty, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_frailty_Results, here('dataRDS', 'mod_frailty_Results.RDS'))




# (B) models for manuscript ----

# relevel imdQ so that least deprived is reference category
modDf_manuscript <- readRDS(here('dataRDS', 'modDf.RDS')) |> 
  mutate(imdQF = factor(imdQF, levels = c('5', '4', '3', '2', '1')))

saveRDS(modDf_manuscript, here('dataRDS', 'modDf_manuscript.RDS'))
# modDf_manuscript <- readRDS(here('dataRDS', 'modDf_manuscript.RDS'))


# 1 main model ----
mod_manuscript <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDf_manuscript,
           family = binomial(link = "logit"))

saveRDS(mod_manuscript, here('dataRDS', 'mod_manuscript.RDS'))

mod_Results_manuscript <- tidy(mod_manuscript, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_Results_manuscript, here('dataRDS', 'mod_Results_manuscript.RDS'))


# 2 main model with grouped-up ethnicity level ----
mod_ethGrpd_manuscript <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicityGrp + icbShortName + 
                        specGrp + podGrp + 
                        s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                        s(der_spell_LoS, bs = 'cr') + 
                        s(cci, bs = 'cr') + 
                        s(priorCommContactsAttF2F, bs = 'cr'),
                      data = modDf_manuscript,
                      family = binomial(link = "logit"))

saveRDS(mod_ethGrpd_manuscript, here('dataRDS', 'mod_ethGrpd_manuscript.RDS'))

mod_ethGrpd_Results_manuscript <- tidy(mod_ethGrpd_manuscript, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_ethGrpd_Results_manuscript, here('dataRDS', 'mod_ethGrpd_Results_manuscript.RDS'))


# 3 sensitivity analysis ----

# sens1 - all atts (not just f2f)
modSens1_manuscript <- bam(postCommContactAttYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
                  specGrp + podGrp + 
                  s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                  s(der_spell_LoS, bs = 'cr') + 
                  s(cci, bs = 'cr') + 
                  s(priorCommContactsAtt, bs = 'cr'),
                data = modDf_manuscript,
                family = binomial(link = "logit"))

saveRDS(modSens1_manuscript, here('dataRDS', 'modSens1_manuscript.RDS'))


# sens2 - all apps whether attended or not
modSens2_manuscript <- bam(postCommContactAppYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
                  specGrp + podGrp + 
                  s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                  s(der_spell_LoS, bs = 'cr') + 
                  s(cci, bs = 'cr') + 
                  s(priorCommContactsApp, bs = 'cr'),
                data = modDf_manuscript,
                family = binomial(link = "logit"))

saveRDS(modSens2_manuscript, here('dataRDS', 'modSens2_manuscript.RDS'))



# 4 subgroup analysis----

# fractured femur

modDf_fracFemur_manuscript <- modDf_manuscript %>% 
  filter(grepl('S72', der_diagnosis_all)) %>% 
  filter(podGrp == 'nonElective')

modDf_fracFemur_manuscript %>% 
  summarise(n = n(),
            p = n() / 938581)

modDf_fracFemur_manuscript %>% 
  group_by(postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n))

saveRDS(modDf_fracFemur_manuscript, here('dataRDS', 'modDf_fracFemur_manuscript.RDS'))

mod_fracFemur_manuscript <- gam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicityGrp + 
                       s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                       s(der_spell_LoS, bs = 'cr') + 
                       s(cci, bs = 'cr') + 
                       s(priorCommContactsAttF2F, bs = 'cr'),
                     data = modDf_fracFemur_manuscript,
                     family = binomial(link = "logit"))



saveRDS(mod_fracFemur_manuscript, here('dataRDS', 'mod_fracFemur_manuscript.RDS'))

mod_fracFemur_Results_manuscript <- tidy(mod_fracFemur_manuscript, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_fracFemur_Results_manuscript, here('dataRDS', 'mod_fracFemur_Results_manuscript.RDS'))




# frailty
# define as frailty score >  15, using method described by 
# Gilbert et al, 2018 (https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(18)30668-8/fulltext)

modDf_frailty_manuscript <- modDf_manuscript %>% 
  filter(podGrp == 'nonElective') %>% 
  filter(Der_Age_at_CDS_Activity_Date >= 75) %>% 
  mutate(frailtyScore = ifelse(grepl('F00', der_diagnosis_all), 7.1, 0) +
           ifelse(grepl('G81', der_diagnosis_all), 4.4, 0) +
           ifelse(grepl('G30', der_diagnosis_all), 4.0, 0) +
           ifelse(grepl('I69', der_diagnosis_all), 3.7, 0) +
           ifelse(grepl('R29', der_diagnosis_all), 3.6, 0) +
           ifelse(grepl('N39', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('F05', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('W19', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('S00', der_diagnosis_all), 3.2, 0) +
           ifelse(grepl('R31', der_diagnosis_all), 3.0, 0) +
           ifelse(grepl('B86', der_diagnosis_all), 2.9, 0) +
           ifelse(grepl('R41', der_diagnosis_all), 2.7, 0) +
           ifelse(grepl('R26', der_diagnosis_all), 2.6, 0) +
           ifelse(grepl('I67', der_diagnosis_all), 2.6, 0) +
           ifelse(grepl('R56', der_diagnosis_all), 2.6, 0) +
           ifelse(grepl('R40', der_diagnosis_all), 2.5, 0) +
           ifelse(grepl('T83', der_diagnosis_all), 2.4, 0) +
           ifelse(grepl('S06', der_diagnosis_all), 2.4, 0) +
           
           ifelse(grepl('S42', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('E87', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('M25', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('E86', der_diagnosis_all), 2.3, 0) +
           ifelse(grepl('R54', der_diagnosis_all), 2.2, 0) +
           ifelse(grepl('Z50', der_diagnosis_all), 2.1, 0) +
           ifelse(grepl('F03', der_diagnosis_all), 2.1, 0) +
           ifelse(grepl('W18', der_diagnosis_all), 2.1, 0) +
           ifelse(grepl('Z75', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('F01', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('S80', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('L03', der_diagnosis_all), 2.0, 0) +
           ifelse(grepl('H54', der_diagnosis_all), 1.9, 0) +
           ifelse(grepl('E53', der_diagnosis_all), 1.9, 0) +
           ifelse(grepl('Z60', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('G20', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('R55', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('S22', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('K59', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('N17', der_diagnosis_all), 1.8, 0) +
           ifelse(grepl('L89', der_diagnosis_all), 1.7, 0) +
           ifelse(grepl('Z22', der_diagnosis_all), 1.7, 0) +
           ifelse(grepl('B95', der_diagnosis_all), 1.7, 0) +
           ifelse(grepl('L97', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('R44', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('K26', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('I95', der_diagnosis_all), 1.6, 0) +
           
           ifelse(grepl('N19', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('A41', der_diagnosis_all), 1.6, 0) +
           ifelse(grepl('Z87', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('J96', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('X59', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('M19', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('G40', der_diagnosis_all), 1.5, 0) +
           ifelse(grepl('M81', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('S72', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('S32', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('E16', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('R94', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('N18', der_diagnosis_all), 1.4, 0) +
           ifelse(grepl('R33', der_diagnosis_all), 1.3, 0) +
           ifelse(grepl('R69', der_diagnosis_all), 1.3, 0) +
           ifelse(grepl('N28', der_diagnosis_all), 1.3, 0) +
           ifelse(grepl('R32', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('G31', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('Y95', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('S09', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('R45', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('G45', der_diagnosis_all), 1.2, 0) +
           ifelse(grepl('Z74', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('M79', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('W06', der_diagnosis_all), 1.1, 0) +
           
           ifelse(grepl('S01', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('A04', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('A09', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('J18', der_diagnosis_all), 1.1, 0) +
           ifelse(grepl('J69', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('R47', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('E55', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('Z93', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('R02', der_diagnosis_all), 1.0, 0) +
           ifelse(grepl('R63', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('H91', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('W10', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('W01', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('E05', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('M41', der_diagnosis_all), 0.9, 0) +
           ifelse(grepl('R13', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('Z99', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('U80', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('M80', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('K92', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('I63', der_diagnosis_all), 0.8, 0) +
           ifelse(grepl('N20', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('F10', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('Y84', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('R00', der_diagnosis_all), 0.7, 0) +
           
           ifelse(grepl('J22', der_diagnosis_all), 0.7, 0) +
           ifelse(grepl('Z73', der_diagnosis_all), 0.6, 0) +
           ifelse(grepl('R79', der_diagnosis_all), 0.6, 0) +
           ifelse(grepl('Z91', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('S51', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('F32', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('M48', der_diagnosis_all), 0.5, 0) +
           ifelse(grepl('E83', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('M15', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('D64', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('L08', der_diagnosis_all), 0.4, 0) +
           ifelse(grepl('R11', der_diagnosis_all), 0.3, 0) +
           ifelse(grepl('K52', der_diagnosis_all), 0.3, 0) +
           ifelse(grepl('R50', der_diagnosis_all), 0.1, 0)) %>% 
  filter(frailtyScore > 15)


saveRDS(modDf_frailty_manuscript, here('dataRDS', 'modDf_frailty_manuscript.RDS'))

modDf_frailty_manuscript %>% 
  summarise(n = n(),
            p = n() / 938581)

modDf_frailty_manuscript %>% 
  group_by(postCommContactAttF2FYN) %>% 
  summarise(n = n()) %>% 
  mutate(p = n / sum(n))

mod_frailty_manuscript <- gam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicityGrp + 
                     s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                     s(der_spell_LoS, bs = 'cr') + 
                     s(cci, bs = 'cr') + 
                     s(priorCommContactsAttF2F, bs = 'cr'),
                   data = modDf_frailty_manuscript,
                   family = binomial(link = "logit"))

saveRDS(mod_frailty_manuscript, here('dataRDS', 'mod_frailty_manuscript.RDS'))

mod_frailty_Results_manuscript <- tidy(mod_frailty_manuscript, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_frailty_Results_manuscript, here('dataRDS', 'mod_frailty_Results_manuscript.RDS'))