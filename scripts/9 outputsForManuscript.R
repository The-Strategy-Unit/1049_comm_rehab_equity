# this script generates outputs for a journal manuscript on 
# equity of access to  community services after discharge from hospital

# 0 set_up ----

library(tidyverse)
library(here)
library(broom)
library(mgcv)

# supporting text ----


# get the number of patient discharges and uniqiue patients in the dataset
modDf <- readRDS(here('dataRDS', 'modDf.RDS'))

modDf |> 
  nrow()
# 938,581 patient discharges

modDf |> 
  dplyr::select(der_pseudo_nhs_number) |> 
  unique() |> 
  nrow()
# 726,924 unique patients

modDf |> 
  group_by(der_pseudo_nhs_number) |> 
  summarise(n = n()) |> 
  filter(n > 1) |> 
  nrow()
# 130,329 (13.9%) patients represented more than once in study sample





#rm(modDf)


# get data about incomploteness of attendornot and consultation_medium variables
commContactsCompleteness2vars <- readRDS(file = here('dataRDS', 'commContactsCompleteness2vars.RDS'))

commContactsCompleteness2vars |> 
  mutate(validAttendOrNot = case_when(is.na(AttendOrNot) ~ 'not valid',
                                      AttendOrNot %in% c('0', '1', 'X') ~ 'not valid',
                                      AttendOrNot %in% c('2', '3', '4', '5', '6', '7') ~ 'valid',
                                      TRUE ~ 'not valid')) |> 
  group_by(validAttendOrNot) |> 
  summarise(atts = sum(atts)) |> 
  mutate(pAtts = atts / sum(atts))
 # 16.2% of contacts did not have a valid recorded attendance status

commContactsCompleteness2vars |> 
mutate(validConsultationMedium = case_when(is.na(Consultation_MediumUsed) ~ 'not valid',
                                      Consultation_MediumUsed %in% c('00', '07', '09', '11', '97') ~ 'not valid',
                                      Consultation_MediumUsed %in% c('01', '02', '03', '04', '05', '06', '98') ~ 'valid',
                                      TRUE ~ 'not valid')) |> 
  group_by(validConsultationMedium) |> 
  summarise(atts = sum(atts)) |> 
  mutate(pAtts = atts / sum(atts))
# 13.3% of contacts did not have a valid recorded consultation medium

rm(commContactsCompleteness2vars)


# table 1 ----
table1_manuscript <- modDf %>%
  group_by(sexMf) %>% 
  summarise(n = n(),
            postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
  mutate(p = n / sum(n),
         pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) |> 
  mutate(grp = 'sex') %>% 
  rename(category = sexMf) %>% 
  bind_rows(modDf %>%
              group_by(ageGrp) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(grp = 'age group') %>% 
              rename(category = ageGrp)) %>% 
  bind_rows(modDf %>%
              group_by(imdQF) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(grp = 'Deprivation quintile') %>% 
              rename(category = imdQF)) %>% 
  bind_rows(modDf %>%
              group_by(ethnicity) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(grp = 'ethnicity') %>% 
              rename(category = ethnicity)) %>% 
  bind_rows(modDf %>%
              group_by(podGrp) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(grp = 'admission method') %>% 
              rename(category = podGrp)) |> 
  bind_rows(modDf %>%
              group_by(cciGrp) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>%
              mutate(grp = 'Charlson comorbidity score') %>% 
              rename(category = cciGrp)) %>% 
  bind_rows(modDf %>%
              group_by(loSGrp) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(grp = 'length of stay') %>% 
              rename(category = loSGrp)) %>%   
  bind_rows(modDf %>%
              group_by(priorCommContactAttF2FGrp) %>% 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(grp = 'Comm services contact before admission') %>% 
              rename(category = priorCommContactAttF2FGrp)) |> 
  bind_rows(modDf %>%
              mutate(fracFemur = case_when(grepl('S72', der_diagnosis_all) &
                                             podGrp == 'nonElective' ~ 'fracFemur',
                                           TRUE ~ 'n')) |> 
              group_by(fracFemur) |> 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>%
              mutate(grp = 'fracFemur') %>% 
              rename(category = fracFemur) |> 
              filter(category == 'fracFemur')) |> 
  bind_rows(modDf %>%
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
                       ifelse(grepl('R50', der_diagnosis_all), 0.1, 0)) |> 
              mutate(frail = case_when(frailtyScore > 15 &
                                         podGrp == 'nonElective' &
                                         Der_Age_at_CDS_Activity_Date >= 75~ 'frail',
                                           TRUE ~ 'n')) |> 
              group_by(frail) |> 
              summarise(n = n(),
                        postCommContactAttF2FYN = sum(postCommContactAttF2FYN)) %>% 
              mutate(p = n / sum(n),
                     pPostCommContactAttF2FYN = postCommContactAttF2FYN / n) %>% 
              mutate(p = n / sum(n)) %>% 
              mutate(grp = 'frail') %>% 
              rename(category = frail) |> 
              filter(category == 'frail')) |> 
  mutate(p100 = 100 * p) %>% 
  select(grp, category, n, p, p100, pPostCommContactAttF2FYN)


write.csv(table1_manuscript, here('tables', 'table1_manuscript.csv'), row.names = FALSE)

rm(modDf, table1_manuscript)


# table 2 ----

mod_Results_manuscript <- readRDS(here('dataRDS', 'mod_Results_manuscript.RDS'))
mod_ethGrpd_Results_manuscript <- readRDS(here('dataRDS', 'mod_ethGrpd_Results_manuscript.RDS'))
mod_fracFemur_Results_manuscript <- readRDS(here('dataRDS', 'mod_fracFemur_Results_manuscript.RDS'))
mod_frailty_Results_manuscript <- readRDS(here('dataRDS', 'mod_frailty_Results_manuscript.RDS'))


mod_Results_manuscript_tidy <- mod_Results_manuscript |> 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth')) %>% 
  bind_rows(data.frame(term = c('imdQF5 - least deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)'),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       statistics = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3))) |> 
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) %>% 
  mutate(termGrp = case_when(substr(term, 1, 3) == 'imd' ~ 'IMD',
                             substr(term, 1, 3) == 'sex' ~ 'Sex',
                             substr(term, 1, 3) == 'eth' ~ 'Ethnicity',
                             TRUE ~ 'check')) %>% 
  mutate(termLabel = case_when(substr(term, 1, 3) == 'imd' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'sex' ~ substr(term, 6, nchar(term)),
                               substr(term, 1, 3) == 'eth' ~ substr(term, 10, nchar(term)),
                               TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) |> 
  mutate(termLabel = factor(termLabel, levels = c('5 - least deprived (ref)', '4', '3', '2', '1',
                                                  'male (ref)', 'female',
                                                  'White (ref)',
                                                  'White - British (ref)',
                                                  'White - Irish', 
                                                  'White - other groups', 
                                                  'Asian', 
                                                  'Asian - Indian', 
                                                  'Asian - Pakistani', 
                                                  'Asian - Bangladeshi', 
                                                  'Asian - other groups', 
                                                  'Black', 
                                                  'Black - Caribbean', 
                                                  'Black - African', 
                                                  'Black - other groups', 
                                                  'Mixed', 
                                                  'Mixed - White & Black Caribbean', 
                                                  'Mixed - White & Black African', 
                                                  'Mixed - White & Asian', 
                                                  'Mixed - other groups',
                                                  'Other groups + Chinese',
                                                  'Chinese', 
                                                  'Other groups',
                                                  'Not stated / known'))) |> 
  dplyr::select(covariate = termGrp,
                category = termLabel,
                all_adjustedOr = or,
                all_lcl95 = lcl95, 
                all_ucl95 = ucl95, 
                all_p.value = p.value) |> 
  arrange(covariate, category)


mod_ethGrpd_Results_manuscript_tidy <- mod_ethGrpd_Results_manuscript |> 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth')) %>% 
  bind_rows(data.frame(term = c('imdQF5 - least deprived (ref)', 'sexMfmale (ref)', 'ethnicityGrpWhite (ref)'),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       statistics = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3))) |> 
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
                               TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) |> 
  mutate(termLabel = factor(termLabel, levels = c('5 - least deprived (ref)', '4', '3', '2', '1',
                                                  'male (ref)', 'female',
                                                  'White (ref)',
                                                  'White - British (ref)',
                                                  'White - Irish', 
                                                  'White - other groups', 
                                                  'Asian', 
                                                  'Asian - Indian', 
                                                  'Asian - Pakistani', 
                                                  'Asian - Bangladeshi', 
                                                  'Asian - other groups', 
                                                  'Black', 
                                                  'Black - Caribbean', 
                                                  'Black - African', 
                                                  'Black - other groups', 
                                                  'Mixed heritage', 
                                                  'Mixed - White & Black Caribbean', 
                                                  'Mixed - White & Black African', 
                                                  'Mixed - White & Asian', 
                                                  'Mixed - other groups',
                                                  'Other groups + Chinese',
                                                  'Chinese', 
                                                  'Other groups',
                                                  'Not stated / known'))) |> 
  dplyr::select(covariate = termGrp,
                category = termLabel,
                all_ethGrpd_adjustedOr = or,
                all_ethGrpd_lcl95 = lcl95, 
                all_ethGrpd_ucl95 = ucl95, 
                all_ethGrpd_p.value = p.value) |> 
  arrange(covariate, category)                              



mod_fracFemur_Results_manuscript_tidy <- mod_fracFemur_Results_manuscript |> 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth')) %>% 
  bind_rows(data.frame(term = c('imdQF5 - least deprived (ref)', 'sexMfmale (ref)', 'ethnicityGrpWhite (ref)'),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       statistics = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3))) |> 
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
                               TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) |> 
  mutate(termLabel = factor(termLabel, levels = c('5 - least deprived (ref)', '4', '3', '2', '1',
                                                  'male (ref)', 'female',
                                                  'White (ref)',
                                                  'White - British (ref)',
                                                  'White - Irish', 
                                                  'White - other groups', 
                                                  'Asian', 
                                                  'Asian - Indian', 
                                                  'Asian - Pakistani', 
                                                  'Asian - Bangladeshi', 
                                                  'Asian - other groups', 
                                                  'Black', 
                                                  'Black - Caribbean', 
                                                  'Black - African', 
                                                  'Black - other groups', 
                                                  'Mixed heritage', 
                                                  'Mixed - White & Black Caribbean', 
                                                  'Mixed - White & Black African', 
                                                  'Mixed - White & Asian', 
                                                  'Mixed - other groups',
                                                  'Other groups + Chinese',
                                                  'Chinese', 
                                                  'Other groups',
                                                  'Not stated / known'))) |> 
  dplyr::select(covariate = termGrp,
                category = termLabel,
                fracFemur_adjustedOr = or,
                fracFemur_lcl95 = lcl95, 
                fracFemur_ucl95 = ucl95, 
                fracFemur_p.value = p.value) |> 
  arrange(covariate, category)   



mod_frailty_Results_manuscript_tidy <- mod_frailty_Results_manuscript |> 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth')) %>% 
  bind_rows(data.frame(term = c('imdQF5 - least deprived (ref)', 'sexMfmale (ref)', 'ethnicityGrpWhite (ref)'),
                       estimate = rep(0, 3),
                       std.error = rep(NA_real_, 3),
                       statistics = rep(NA_real_, 3),
                       p.value = rep(NA_real_, 3))) |> 
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
                               TRUE ~ 'check')) %>% 
  mutate(termGrp = factor(termGrp, levels = c('IMD', 'Sex', 'Ethnicity'))) |> 
  mutate(termLabel = factor(termLabel, levels = c('5 - least deprived (ref)', '4', '3', '2', '1',
                                                  'male (ref)', 'female',
                                                  'White (ref)',
                                                  'White - British (ref)',
                                                  'White - Irish', 
                                                  'White - other groups', 
                                                  'Asian', 
                                                  'Asian - Indian', 
                                                  'Asian - Pakistani', 
                                                  'Asian - Bangladeshi', 
                                                  'Asian - other groups', 
                                                  'Black', 
                                                  'Black - Caribbean', 
                                                  'Black - African', 
                                                  'Black - other groups', 
                                                  'Mixed heritage', 
                                                  'Mixed - White & Black Caribbean', 
                                                  'Mixed - White & Black African', 
                                                  'Mixed - White & Asian', 
                                                  'Mixed - other groups',
                                                  'Other groups + Chinese',
                                                  'Chinese', 
                                                  'Other groups',
                                                  'Not stated / known'))) |> 
  dplyr::select(covariate = termGrp,
                category = termLabel,
                frailty_adjustedOr = or,
                frailty_lcl95 = lcl95, 
                frailty_ucl95 = ucl95, 
                frailty_p.value = p.value) |> 
  arrange(covariate, category)  




table2_manuscript  <- mod_Results_manuscript_tidy |> 
  full_join(mod_ethGrpd_Results_manuscript_tidy,
            join_by(covariate, category)) |> 
  full_join(mod_fracFemur_Results_manuscript_tidy,
            join_by(covariate, category)) |> 
  full_join(mod_frailty_Results_manuscript_tidy,
            join_by(covariate, category)) |> 
  mutate(covariate = factor(covariate, levels = c('IMD', 'Sex', 'Ethnicity'))) |> 
  mutate(category = factor(category, levels = c('5 - least deprived (ref)', '4', '3', '2', '1',
                                                  'male (ref)', 'female',
                                                  'White (ref)',
                                                  'White - British (ref)',
                                                  'White - Irish', 
                                                  'White - other groups', 
                                                  'Asian', 
                                                  'Asian - Indian', 
                                                  'Asian - Pakistani', 
                                                  'Asian - Bangladeshi', 
                                                  'Asian - other groups', 
                                                  'Black', 
                                                  'Black - Caribbean', 
                                                  'Black - African', 
                                                  'Black - other groups', 
                                                  'Mixed heritage', 
                                                  'Mixed - White & Black Caribbean', 
                                                  'Mixed - White & Black African', 
                                                  'Mixed - White & Asian', 
                                                  'Mixed - other groups',
                                                  'Other groups + Chinese',
                                                  'Chinese', 
                                                  'Other groups',
                                                  'Not stated / known'))) |>
  arrange(covariate, category)

write.csv(table2_manuscript, here('tables', 'table2_manuscript.csv'), row.names = FALSE)

rm(mod_Results_manuscript, mod_Results_manuscript_tidy)
rm(mod_ethGrpd_Results_manuscript, mod_ethGrpd_Results_manuscript_tidy)
rm(mod_fracFemur_Results_manuscript, mod_fracFemur_Results_manuscript_tidy)
rm(mod_frailty_Results_manuscript, mod_frailty_Results_manuscript_tidy)

rm(table2_manuscript)
