# this script builds models to explore equity of access to community services after discharge from hospital
# this script needs to be run in the NCDR data science environment

# 0 set_up ----

library(tidyverse)
library(here)
library(broom)
library(mgcv)

comRehabEquity <- readRDS(file = here('dataRDS', 'comRehabEquity.RDS'))

icbCodesNames <- read.csv(file = here('dataRaw', 'icbCodesNames.csv'), header = TRUE)

# 1 create dataframe for models ----
modDf <- comRehabEquity %>% 
  filter(Der_Age_at_CDS_Activity_Date >= 65) %>% 
  filter(Der_Age_at_CDS_Activity_Date <= 110) %>% 
  filter(sex %in% c(1, 2)) %>% 
  filter(!is.na(imd2019Dec)) %>% 
  filter(der_spell_LoS <= 60) %>% 
  filter(!is.na(der_admit_treatment_function_code)) %>% 
  filter(der_national_pod_code != 'APCUNK') %>% 
  filter(STP21CD != 'E54000041') %>% 
  left_join(icbCodesNames, by = c('STP21CD' = 'icbCode')) %>% 
  mutate(ageGrp = case_when(Der_Age_at_CDS_Activity_Date < 70 ~ '65-69y',
                            Der_Age_at_CDS_Activity_Date < 75 ~ '70-74y',
                            Der_Age_at_CDS_Activity_Date < 80 ~ '75-79y',
                            Der_Age_at_CDS_Activity_Date < 85 ~ '80-84y',
                            Der_Age_at_CDS_Activity_Date < 90 ~ '85-89y',
                            TRUE ~ '90+y' )) %>% 
  mutate(sexMf = case_when(sex == 1 ~ 'male',
                           TRUE ~ 'female')) %>% 
  mutate(ethnicity = case_when(ethnic_group == '99' ~ 'Not stated / known',
                               substr(ethnic_group, 1, 1) == 'Z' ~ 'NSNK',
                               substr(ethnic_group, 1, 1) == 'A' ~ 'White - British',
                               substr(ethnic_group, 1, 1) == 'B' ~ 'White - Irish',
                               substr(ethnic_group, 1, 1) == 'C' ~ 'White - other groups',
                               substr(ethnic_group, 1, 1) == 'D' ~ 'Mixed - White & Black Caribbean',
                               substr(ethnic_group, 1, 1) == 'E' ~ 'Mixed - White & Black African',
                               substr(ethnic_group, 1, 1) == 'F' ~ 'Mixed - White & Asian',
                               substr(ethnic_group, 1, 1) == 'G' ~ 'Mixed - other groups',
                               substr(ethnic_group, 1, 1) == 'H' ~ 'Asian - Indian',
                               substr(ethnic_group, 1, 1) == 'J' ~ 'Asian - Pakistani',
                               substr(ethnic_group, 1, 1) == 'K' ~ 'Asian - Bangladeshi',
                               substr(ethnic_group, 1, 1) == 'L' ~ 'Asian - other groups',
                               substr(ethnic_group, 1, 1) == 'M' ~ 'Black - Caribbean',
                               substr(ethnic_group, 1, 1) == 'N' ~ 'Black - African',
                               substr(ethnic_group, 1, 1) == 'P' ~ 'Black - other groups',
                               substr(ethnic_group, 1, 1) == 'R' ~ 'Chinese',
                               substr(ethnic_group, 1, 1) == 'S' ~ 'Other groups',
                               is.na(ethnic_group) ~ 'Not stated / known',
                               TRUE ~ substr(ethnic_group, 1, 1))) %>% 
  mutate(ethnicityGrp = case_when(ethnicity %in% c('A', 'B', 'C') ~ 'White',
                                  ethnicity %in% c('D', 'E', 'F', 'G') ~ 'Mixed Heritage',
                                  ethnicity %in% c('H', 'J', 'K', 'L') ~ 'Asian',
                                  ethnicity %in% c('M', 'N', 'P') ~ 'Black',
                                  ethnicity %in% c('R', 'S') ~ 'other',
                                  ethnicity %in% c('Not stated / known') ~ 'Not stated / known',
                                  TRUE ~ 'check')) %>% 
  mutate(imdQ = case_when(imd2019Dec %in% c(1, 2) ~ 1,
                          imd2019Dec %in% c(3, 4) ~ 2,
                          imd2019Dec %in% c(5, 6) ~ 3,
                          imd2019Dec %in% c(7, 8) ~ 4,
                          imd2019Dec %in% c(9, 10) ~ 5,
                          TRUE ~ 0)) %>% 
  mutate(loSGrp = case_when(der_spell_LoS == 0 ~ '00d',
                            der_spell_LoS <= 2 ~ '01_02d',
                            der_spell_LoS <= 7 ~ '03_07d',
                            der_spell_LoS <= 14 ~ '08_14d',
                            der_spell_LoS <= 30 ~ '15_30d',
                            der_spell_LoS <= 60 ~ '31_60d',
                            is.na(der_spell_LoS) ~ 'na',
                            TRUE ~ '31+d')) %>% 
  mutate(cci_Age = case_when(Der_Age_at_CDS_Activity_Date < 50 ~ 0,
                             Der_Age_at_CDS_Activity_Date < 60 ~ 1,
                             Der_Age_at_CDS_Activity_Date < 70 ~ 2,
                             Der_Age_at_CDS_Activity_Date < 80 ~ 3,
                             TRUE ~ 4),
         cci_Mi = case_when(str_detect(der_diagnosis_all, 'I21') ~ 1,
                            str_detect(der_diagnosis_all, 'I22') ~ 1,
                            str_detect(der_diagnosis_all, 'I252') ~ 1,
                            TRUE ~ 0),
         cci_Chf = case_when(str_detect(der_diagnosis_all, 'I099') ~ 1,
                             str_detect(der_diagnosis_all, 'I110') ~ 1,
                             str_detect(der_diagnosis_all, 'I130') ~ 1,
                             str_detect(der_diagnosis_all, 'I132') ~ 1,
                             str_detect(der_diagnosis_all, 'I255') ~ 1,
                             str_detect(der_diagnosis_all, 'I420') ~ 1,
                             str_detect(der_diagnosis_all, 'I425') ~ 1,
                             str_detect(der_diagnosis_all, 'I426') ~ 1,
                             str_detect(der_diagnosis_all, 'I427') ~ 1,
                             str_detect(der_diagnosis_all, 'I428') ~ 1,
                             str_detect(der_diagnosis_all, 'I429') ~ 1,
                             str_detect(der_diagnosis_all, 'I43') ~ 1,
                             str_detect(der_diagnosis_all, 'I50') ~ 1,
                             str_detect(der_diagnosis_all, 'I290') ~ 1,
                             TRUE ~ 0),
         cch_Pvd = case_when(str_detect(der_diagnosis_all, 'I70') ~ 1,
                             str_detect(der_diagnosis_all, 'I71') ~ 1,
                             str_detect(der_diagnosis_all, 'I731') ~ 1,
                             str_detect(der_diagnosis_all, 'I738') ~ 1,
                             str_detect(der_diagnosis_all, 'I739') ~ 1,
                             str_detect(der_diagnosis_all, 'I771') ~ 1,
                             str_detect(der_diagnosis_all, 'I790') ~ 1,
                             str_detect(der_diagnosis_all, 'I792') ~ 1,
                             str_detect(der_diagnosis_all, 'K551') ~ 1,
                             str_detect(der_diagnosis_all, 'K558') ~ 1,
                             str_detect(der_diagnosis_all, 'K559') ~ 1,
                             str_detect(der_diagnosis_all, 'Z958') ~ 1,
                             str_detect(der_diagnosis_all, 'Z959') ~ 1,
                             TRUE ~ 0),
         cci_Cbvd = case_when(str_detect(der_diagnosis_all, 'G45') ~ 1,
                              str_detect(der_diagnosis_all, 'G46') ~ 1,
                              str_detect(der_diagnosis_all, 'H340') ~ 1,
                              str_detect(der_diagnosis_all, 'I6') ~ 1,
                              TRUE ~ 0),
         cci_Dem = case_when(str_detect(der_diagnosis_all, 'F00') ~ 1,
                             str_detect(der_diagnosis_all, 'F03') ~ 1,
                             str_detect(der_diagnosis_all, 'F05') ~ 1,
                             str_detect(der_diagnosis_all, 'G30') ~ 1,
                             str_detect(der_diagnosis_all, 'G311') ~ 1,
                             TRUE ~ 0),
         cci_Copd = case_when(str_detect(der_diagnosis_all, 'I278') ~ 1,
                              str_detect(der_diagnosis_all, 'I279') ~ 1,
                              str_detect(der_diagnosis_all, 'J40') ~ 1,
                              str_detect(der_diagnosis_all, 'J41') ~ 1,
                              str_detect(der_diagnosis_all, 'J42') ~ 1,
                              str_detect(der_diagnosis_all, 'J43') ~ 1,
                              str_detect(der_diagnosis_all, 'J44') ~ 1,
                              str_detect(der_diagnosis_all, 'J45') ~ 1,
                              str_detect(der_diagnosis_all, 'J46') ~ 1,
                              str_detect(der_diagnosis_all, 'J47') ~ 1,
                              str_detect(der_diagnosis_all, 'J60') ~ 1,
                              str_detect(der_diagnosis_all, 'J61') ~ 1,
                              str_detect(der_diagnosis_all, 'J62') ~ 1,
                              str_detect(der_diagnosis_all, 'J63') ~ 1,
                              str_detect(der_diagnosis_all, 'J64') ~ 1,
                              str_detect(der_diagnosis_all, 'J65') ~ 1,
                              str_detect(der_diagnosis_all, 'J66') ~ 1,
                              str_detect(der_diagnosis_all, 'J67') ~ 1,
                              str_detect(der_diagnosis_all, 'J684') ~ 1,
                              str_detect(der_diagnosis_all, 'J701') ~ 1,
                              str_detect(der_diagnosis_all, 'J703') ~ 1,
                              TRUE ~ 0),
         cci_Rhd = case_when(str_detect(der_diagnosis_all, 'M05') ~ 1,
                             str_detect(der_diagnosis_all, 'M06') ~ 1,
                             str_detect(der_diagnosis_all, 'M315') ~ 1,
                             str_detect(der_diagnosis_all, 'M32') ~ 1,
                             str_detect(der_diagnosis_all, 'M33') ~ 1,
                             str_detect(der_diagnosis_all, 'M34') ~ 1,
                             str_detect(der_diagnosis_all, 'M351') ~ 1,
                             str_detect(der_diagnosis_all, 'M353') ~ 1,
                             str_detect(der_diagnosis_all, 'M360') ~ 1,
                             TRUE ~ 0),
         cci_Pud = case_when(str_detect(der_diagnosis_all, 'K25') ~ 1,
                             str_detect(der_diagnosis_all, 'K26') ~ 1,
                             str_detect(der_diagnosis_all, 'K27') ~ 1,
                             str_detect(der_diagnosis_all, 'K28') ~ 1,
                             TRUE ~ 0),
         cci_Liver = case_when(str_detect(der_diagnosis_all, 'I850') ~ 3,
                               str_detect(der_diagnosis_all, 'I859') ~ 3,
                               str_detect(der_diagnosis_all, 'I864') ~ 3,
                               str_detect(der_diagnosis_all, 'I982') ~ 3,
                               str_detect(der_diagnosis_all, 'K704') ~ 3,
                               str_detect(der_diagnosis_all, 'K711') ~ 3,
                               str_detect(der_diagnosis_all, 'K721') ~ 3,
                               str_detect(der_diagnosis_all, 'K729') ~ 3,
                               str_detect(der_diagnosis_all, 'K765') ~ 3,
                               str_detect(der_diagnosis_all, 'K766') ~ 3,
                               str_detect(der_diagnosis_all, 'K767') ~ 3,
                               str_detect(der_diagnosis_all, 'B18') ~ 1,
                               str_detect(der_diagnosis_all, 'K700') ~ 1,
                               str_detect(der_diagnosis_all, 'K701') ~ 1,
                               str_detect(der_diagnosis_all, 'K702') ~ 1,
                               str_detect(der_diagnosis_all, 'K703') ~ 1,
                               str_detect(der_diagnosis_all, 'K709') ~ 1,
                               str_detect(der_diagnosis_all, 'K713') ~ 1,
                               str_detect(der_diagnosis_all, 'K714') ~ 1,
                               str_detect(der_diagnosis_all, 'K715') ~ 1,
                               str_detect(der_diagnosis_all, 'K717') ~ 1,
                               str_detect(der_diagnosis_all, 'K73') ~ 1,
                               str_detect(der_diagnosis_all, 'K74') ~ 1,
                               str_detect(der_diagnosis_all, 'K760') ~ 1,
                               str_detect(der_diagnosis_all, 'K762') ~ 1,
                               str_detect(der_diagnosis_all, 'K763') ~ 1,
                               str_detect(der_diagnosis_all, 'K764') ~ 1,
                               str_detect(der_diagnosis_all, 'K768') ~ 1,
                               str_detect(der_diagnosis_all, 'K769') ~ 1,
                               str_detect(der_diagnosis_all, 'Z944') ~ 1,
                               TRUE ~ 0),
         cci_Diab = case_when(str_detect(der_diagnosis_all, 'E102') ~ 2,
                              str_detect(der_diagnosis_all, 'E103') ~ 2,
                              str_detect(der_diagnosis_all, 'E104') ~ 2,
                              str_detect(der_diagnosis_all, 'E105') ~ 2,
                              str_detect(der_diagnosis_all, 'E107') ~ 2,
                              str_detect(der_diagnosis_all, 'E112') ~ 2,
                              str_detect(der_diagnosis_all, 'E113') ~ 2,
                              str_detect(der_diagnosis_all, 'E114') ~ 2,
                              str_detect(der_diagnosis_all, 'E115') ~ 2,
                              str_detect(der_diagnosis_all, 'E117') ~ 2,
                              str_detect(der_diagnosis_all, 'E122') ~ 2,
                              str_detect(der_diagnosis_all, 'E123') ~ 2,
                              str_detect(der_diagnosis_all, 'E124') ~ 2,
                              str_detect(der_diagnosis_all, 'E125') ~ 2,
                              str_detect(der_diagnosis_all, 'E127') ~ 2,
                              str_detect(der_diagnosis_all, 'E132') ~ 2,
                              str_detect(der_diagnosis_all, 'E133') ~ 2,
                              str_detect(der_diagnosis_all, 'E134') ~ 2,
                              str_detect(der_diagnosis_all, 'E135') ~ 2,
                              str_detect(der_diagnosis_all, 'E137') ~ 2,
                              str_detect(der_diagnosis_all, 'E142') ~ 2,
                              str_detect(der_diagnosis_all, 'E143') ~ 2,
                              str_detect(der_diagnosis_all, 'E144') ~ 2,
                              str_detect(der_diagnosis_all, 'E145') ~ 2,
                              str_detect(der_diagnosis_all, 'E147') ~ 2,
                              str_detect(der_diagnosis_all, 'E100') ~ 1,
                              str_detect(der_diagnosis_all, 'E101') ~ 1,
                              str_detect(der_diagnosis_all, 'E106') ~ 1,
                              str_detect(der_diagnosis_all, 'E108') ~ 1,
                              str_detect(der_diagnosis_all, 'E109') ~ 1,
                              str_detect(der_diagnosis_all, 'E110') ~ 1,
                              str_detect(der_diagnosis_all, 'E111') ~ 1,
                              str_detect(der_diagnosis_all, 'E116') ~ 1,
                              str_detect(der_diagnosis_all, 'E118') ~ 1,
                              str_detect(der_diagnosis_all, 'E119') ~ 1,
                              str_detect(der_diagnosis_all, 'E120') ~ 1,
                              str_detect(der_diagnosis_all, 'E121') ~ 1,
                              str_detect(der_diagnosis_all, 'E126') ~ 1,
                              str_detect(der_diagnosis_all, 'E128') ~ 1,
                              str_detect(der_diagnosis_all, 'E129') ~ 1,
                              str_detect(der_diagnosis_all, 'E130') ~ 1,
                              str_detect(der_diagnosis_all, 'E131') ~ 1,
                              str_detect(der_diagnosis_all, 'E136') ~ 1,
                              str_detect(der_diagnosis_all, 'E138') ~ 1,
                              str_detect(der_diagnosis_all, 'E139') ~ 1,
                              str_detect(der_diagnosis_all, 'E140') ~ 1,
                              str_detect(der_diagnosis_all, 'E141') ~ 1,
                              str_detect(der_diagnosis_all, 'E146') ~ 1,
                              str_detect(der_diagnosis_all, 'E148') ~ 1,
                              str_detect(der_diagnosis_all, 'E149') ~ 1,
                              TRUE ~ 0),
         cci_Hemip = case_when(str_detect(der_diagnosis_all, 'G041') ~ 2,
                               str_detect(der_diagnosis_all, 'G114') ~ 2,
                               str_detect(der_diagnosis_all, 'G801') ~ 2,
                               str_detect(der_diagnosis_all, 'G802') ~ 2,
                               str_detect(der_diagnosis_all, 'G81') ~ 2,
                               str_detect(der_diagnosis_all, 'G82') ~ 2,
                               str_detect(der_diagnosis_all, 'G830') ~ 2,
                               str_detect(der_diagnosis_all, 'G831') ~ 2,
                               str_detect(der_diagnosis_all, 'G832') ~ 2,
                               str_detect(der_diagnosis_all, 'G833') ~ 2,
                               str_detect(der_diagnosis_all, 'G834') ~ 2,
                               str_detect(der_diagnosis_all, 'G839') ~ 2,
                               TRUE ~ 0),
         cci_Renal = case_when(str_detect(der_diagnosis_all, 'I120') ~ 2,
                               str_detect(der_diagnosis_all, 'I131') ~ 2,
                               str_detect(der_diagnosis_all, 'N032') ~ 2,
                               str_detect(der_diagnosis_all, 'N033') ~ 2,
                               str_detect(der_diagnosis_all, 'N034') ~ 2,
                               str_detect(der_diagnosis_all, 'N035') ~ 2,
                               str_detect(der_diagnosis_all, 'N036') ~ 2,
                               str_detect(der_diagnosis_all, 'N037') ~ 2,
                               str_detect(der_diagnosis_all, 'N052') ~ 2,
                               str_detect(der_diagnosis_all, 'N053') ~ 2,
                               str_detect(der_diagnosis_all, 'N054') ~ 2,
                               str_detect(der_diagnosis_all, 'N055') ~ 2,
                               str_detect(der_diagnosis_all, 'N056') ~ 2,
                               str_detect(der_diagnosis_all, 'N057') ~ 2,
                               str_detect(der_diagnosis_all, 'N18') ~ 2,
                               str_detect(der_diagnosis_all, 'N19') ~ 2,
                               str_detect(der_diagnosis_all, 'N250') ~ 2,
                               str_detect(der_diagnosis_all, 'Z490') ~ 2,
                               str_detect(der_diagnosis_all, 'Z491') ~ 2,
                               str_detect(der_diagnosis_all, 'Z492') ~ 2,
                               str_detect(der_diagnosis_all, 'Z940') ~ 2,
                               str_detect(der_diagnosis_all, 'Z992') ~ 2,
                               TRUE ~ 0),
         cci_Cancer = case_when(str_detect(der_diagnosis_all, 'C77') ~ 6,
                                str_detect(der_diagnosis_all, 'C78') ~ 6,
                                str_detect(der_diagnosis_all, 'C79') ~ 6,
                                str_detect(der_diagnosis_all, 'C80') ~ 6,
                                str_detect(der_diagnosis_all, 'C0') ~ 2,
                                str_detect(der_diagnosis_all, 'C1') ~ 2,
                                str_detect(der_diagnosis_all, 'C20') ~ 2,
                                str_detect(der_diagnosis_all, 'C21') ~ 2,
                                str_detect(der_diagnosis_all, 'C22') ~ 2,
                                str_detect(der_diagnosis_all, 'C23') ~ 2,
                                str_detect(der_diagnosis_all, 'C24') ~ 2,
                                str_detect(der_diagnosis_all, 'C25') ~ 2,
                                str_detect(der_diagnosis_all, 'C26') ~ 2,
                                str_detect(der_diagnosis_all, 'C30') ~ 2,
                                str_detect(der_diagnosis_all, 'C31') ~ 2,
                                str_detect(der_diagnosis_all, 'C32') ~ 2,
                                str_detect(der_diagnosis_all, 'C33') ~ 2,
                                str_detect(der_diagnosis_all, 'C34') ~ 2,
                                str_detect(der_diagnosis_all, 'C37') ~ 2,
                                str_detect(der_diagnosis_all, 'C38') ~ 2,
                                str_detect(der_diagnosis_all, 'C39') ~ 2,
                                str_detect(der_diagnosis_all, 'C40') ~ 2,
                                str_detect(der_diagnosis_all, 'C41') ~ 2,
                                str_detect(der_diagnosis_all, 'C43') ~ 2,
                                str_detect(der_diagnosis_all, 'C45') ~ 2,
                                str_detect(der_diagnosis_all, 'C46') ~ 2,
                                str_detect(der_diagnosis_all, 'C47') ~ 2,
                                str_detect(der_diagnosis_all, 'C48') ~ 2,
                                str_detect(der_diagnosis_all, 'C49') ~ 2,
                                str_detect(der_diagnosis_all, 'C50') ~ 2,
                                str_detect(der_diagnosis_all, 'C51') ~ 2,
                                str_detect(der_diagnosis_all, 'C52') ~ 2,
                                str_detect(der_diagnosis_all, 'C53') ~ 2,
                                str_detect(der_diagnosis_all, 'C54') ~ 2,
                                str_detect(der_diagnosis_all, 'C55') ~ 2,
                                str_detect(der_diagnosis_all, 'C56') ~ 2,
                                str_detect(der_diagnosis_all, 'C57') ~ 2,
                                str_detect(der_diagnosis_all, 'C58') ~ 2,
                                str_detect(der_diagnosis_all, 'C6') ~ 2,
                                str_detect(der_diagnosis_all, 'C70') ~ 2,
                                str_detect(der_diagnosis_all, 'C71') ~ 2,
                                str_detect(der_diagnosis_all, 'C72') ~ 2,
                                str_detect(der_diagnosis_all, 'C73') ~ 2,
                                str_detect(der_diagnosis_all, 'C74') ~ 2,
                                str_detect(der_diagnosis_all, 'C75') ~ 2,
                                str_detect(der_diagnosis_all, 'C76') ~ 2,
                                str_detect(der_diagnosis_all, 'C81') ~ 2,
                                str_detect(der_diagnosis_all, 'C82') ~ 2,
                                str_detect(der_diagnosis_all, 'C83') ~ 2,
                                str_detect(der_diagnosis_all, 'C84') ~ 2,
                                str_detect(der_diagnosis_all, 'C85') ~ 2,
                                str_detect(der_diagnosis_all, 'C88') ~ 2,
                                str_detect(der_diagnosis_all, 'C90') ~ 2,
                                str_detect(der_diagnosis_all, 'C91') ~ 2,
                                str_detect(der_diagnosis_all, 'C92') ~ 2,
                                str_detect(der_diagnosis_all, 'C93') ~ 2,
                                str_detect(der_diagnosis_all, 'C94') ~ 2,
                                str_detect(der_diagnosis_all, 'C95') ~ 2,
                                str_detect(der_diagnosis_all, 'C96') ~ 2,
                                str_detect(der_diagnosis_all, 'C97') ~ 2,
                                TRUE ~ 0),
         cci_Aids = case_when(str_detect(der_diagnosis_all, 'B20') ~ 6,
                              str_detect(der_diagnosis_all, 'B21') ~ 6,
                              str_detect(der_diagnosis_all, 'B22') ~ 6,
                              str_detect(der_diagnosis_all, 'B24') ~ 6,
                              TRUE ~ 0)) %>% 
  mutate(cci = cci_Age + cci_Mi + cci_Chf + cci_Cbvd + cci_Dem + cci_Copd + cci_Rhd + 
           cci_Pud + cci_Liver + cci_Diab + cci_Hemip + cci_Renal + cci_Cancer + cci_Aids) %>% 
  mutate(cciGrp = case_when(cci == 0 ~ '0c',
                            cci <= 2 ~ '1_2c',
                            cci <= 4 ~ '3_4c',
                            cci >= 5 ~ '5+c',
                            TRUE ~ 'check')) %>% 
  group_by(der_admit_treatment_function_code) %>% 
  mutate(specGrp = case_when(n() >= 500 ~ der_admit_treatment_function_code,
                             TRUE ~ 'otherSpec')) %>% 
  ungroup() %>% 
  mutate(podGrp = case_when(der_national_pod_code %in% c('DC', 'EL') ~ 'elective',
                            der_national_pod_code %in% c('NEL', 'NELST', 'NELNE') ~ 'nonElective',
                            TRUE ~ 'check')) %>% 
  mutate(priorCommContactAttF2FGrp = case_when(priorCommContactsAttF2F == 0 ~ '00p',
                                         priorCommContactsAttF2F == 1 ~ '01p',
                                         priorCommContactsAttF2F <= 3 ~ '02_03p',
                                         priorCommContactsAttF2F <= 8 ~ '04_08p',
                                         priorCommContactsAttF2F <= 15 ~ '09_15p',
                                         priorCommContactsAttF2F <= 29 ~ '16_29p',
                                         TRUE ~ '30+p')) %>% 
  mutate(priorCommContactAttGrp = case_when(priorCommContactsAtt == 0 ~ '00p',
                                               priorCommContactsAtt == 1 ~ '01p',
                                               priorCommContactsAtt <= 3 ~ '02_03p',
                                               priorCommContactsAtt <= 8 ~ '04_08p',
                                               priorCommContactsAtt <= 15 ~ '09_15p',
                                               priorCommContactsAtt <= 29 ~ '16_29p',
                                               TRUE ~ '30+p')) %>% 
  mutate(priorCommContactAppGrp = case_when(priorCommContactsApp == 0 ~ '00p',
                                               priorCommContactsApp == 1 ~ '01p',
                                               priorCommContactsApp <= 3 ~ '02_03p',
                                               priorCommContactsApp <= 8 ~ '04_08p',
                                               priorCommContactsApp <= 15 ~ '09_15p',
                                               priorCommContactsApp <= 29 ~ '16_29p',
                                               TRUE ~ '30+p')) %>% 
  mutate(postCommContactAttF2FYN = case_when(postCommContactsAttF2F == 0 ~ 0,
                                       TRUE ~ 1)) %>% 
  mutate(postCommContactAttYN = case_when(postCommContactsAtt == 0 ~ 0,
                                             TRUE ~ 1)) %>% 
  mutate(postCommContactAppYN = case_when(postCommContactsApp == 0 ~ 0,
                                          TRUE ~ 1)) %>%   
  mutate(ageGrp = factor(ageGrp, levels = c('65-69y', '70-74y', '75-79y', '80-84y', '85-89y', '90+y', ''))) %>% 
  mutate(sexMf = factor(sexMf, levels = c('male', 'female'))) %>% 
  mutate(ethnicity = factor(ethnicity, levels = c('White - British', 'White - Irish', 'White - other groups',
                                                   'Asian - Indian', 'Asian - Pakistani', 'Asian - Bangladeshi', 'Asian - other groups',
                                                   'Black - Caribbean', 'Black - African', 'Black - other groups',
                                                   'Mixed - White & Black Caribbean', 'Mixed - White & Black African', 
                                                   'Mixed - White & Asian', 'Mixed - other groups', '',
                                                   'Chinese', 'Other groups', 
                                                   'Not stated / known'))) %>% 
  mutate(imdQF = factor(imdQ, levels = c('1', '2', '3', '4', '5'))) %>% 
  mutate(loSGrp = factor(loSGrp, levels = c('00d', '01_02d', '03_07d', '08_14d', '15_30d', '31_60d'))) %>% 
  mutate(podGrp = factor(podGrp, levels = c('nonElective', 'elective'))) %>% 
  mutate(priorCommContactAttF2FGrp = factor(priorCommContactAttF2FGrp, levels = c('00p', '01p', '02_03p',
                                                                                  '04_08p', '09_15p', '16_29p',
                                                                                  '30+p'))) %>% 
  mutate(priorCommContactAttGrp = factor(priorCommContactAttGrp, levels = c('00p', '01p', '02_03p',
                                                                            '04_08p', '09_15p', '16_29p',
                                                                            '30+p'))) %>% 
  mutate(priorCommContactAppGrp = factor(priorCommContactAppGrp, levels = c('00p', '01p', '02_03p',
                                                                            '04_08p', '09_15p', '16_29p',
                                                                            '30+p'))) %>% 
  mutate(specGrp = factor(specGrp)) %>% 
  mutate(specGrp = relevel(specGrp, ref = '300')) %>%
  mutate(cciGrp = factor(cciGrp, levels = c('0c', '1_2c', '3_4c', '5+c'))) %>% 
  mutate(icbShortName = factor(icbShortName)) %>% 
  mutate(icbShortName = relevel(icbShortName, ref = 'Cambridgeshire & Peterborough'))
  
saveRDS(modDf, here('dataRDS', 'modDf.RDS'))
modDf <- readRDS(here('dataRDS', 'modDf.RDS'))
rm(comRehabEquity)



  
# 2 build preliminary model - using grouped variables ----  
modPrelim <- glm(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             ageGrp + loSGrp + cciGrp + specGrp + podGrp + priorCommContactAttF2FGrp,
           data = modDf,
           family = binomial(link = "logit"))

saveRDS(modPrelim, here('dataRDS', 'modPrelim.RDS'))

modPrelim_Results <- tidy(modPrelim, exponentiate = TRUE) 

saveRDS(modPrelim_Results, here('dataRDS', 'modPrelim_Results.RDS'))

modPrelim_Results %>% 
  bind_rows(data.frame(term = c('imdQF1', 'sexMfmale', 'ethnicityWhite - British', 'icbShortNameCambridgeshire & Peterborough', 'ageGrp65-69y', 
                                'loSGrp00d', 'cciGrp1_2c', 'specGrp300', 'podGrpnonElective', 'priorCommContactAttF2FGrp00p' ),
                       estimate = rep(1, 10),
                       std.error = rep(NA_real_, 10),
                       statistics = rep(NA_real_, 10),
                       p.value = rep(NA_real_, 10))) %>% 
  mutate(lcl95 = estimate - 1.96*std.error,
         ucl95 = estimate + 1.96*std.error) %>% 
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
  geom_point(aes(y = termLabel, x = estimate)) +
  scale_x_log10(name ='Odds ratio (log scale)') +
  facet_wrap(~termGrp, scales = 'free')
         



# 3 build final model with gam terms for age, cci, prior comm contacts ----
mod <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDf,
           family = binomial(link = "logit"))

saveRDS(mod, here('dataRDS', 'mod.RDS'))

mod_Results <- tidy(mod, exponentiate = TRUE, parametric = TRUE) 

saveRDS(mod_Results, here('dataRDS', 'mod_Results.RDS'))

mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(1, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(lcl95 = estimate - 1.96*std.error,
         ucl95 = estimate + 1.96*std.error) %>% 
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
  geom_point(aes(y = termLabel, x = estimate, colour = orSigGrp)) +
  scale_x_log10(name ='Odds ratio (log scale)') +
  scale_y_discrete(name = '', limits = rev) +
  scale_colour_manual(values = c('grey', 'green', 'red')) +
  facet_wrap(~termGrp, scales = 'free') +
  theme(legend.position = 'none')

plot(mod, residuals = FALSE, se = FALSE, select = 1)
plot(mod, residuals = FALSE, se = FALSE, select = 2)
plot(mod, residuals = FALSE, se = FALSE, select = 3)
plot(mod, residuals = FALSE, se = FALSE, select = 4)



# 4 create chart outputs for covariates of interest ----
mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(1, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(lcl95 = estimate - 1.96*std.error,
         ucl95 = estimate + 1.96*std.error) %>% 
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
  geom_point(aes(y = termLabel, x = estimate, colour = orSigGrp)) +
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
                       estimate = rep(1, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(lcl95 = estimate - 1.96*std.error,
         ucl95 = estimate + 1.96*std.error) %>% 
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
  geom_point(aes(y = termLabel, x = estimate, colour = orSigGrp)) +
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
                       estimate = rep(1, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(lcl95 = estimate - 1.96*std.error,
         ucl95 = estimate + 1.96*std.error) %>% 
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
  geom_point(aes(y = termLabel, x = estimate, colour = orSigGrp)) +
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
                       estimate = rep(1, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(lcl95 = estimate - 1.96*std.error,
         ucl95 = estimate + 1.96*std.error) %>% 
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
  geom_point(aes(y = termLabel, x = estimate, colour = orSigGrp)) +
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
  

modSens3 <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDfSens3,
           family = binomial(link = "logit"))

saveRDS(modDfSens3, here('dataRDS', 'modDfSens3.RDS'))



mod_Results <- tidy(mod, exponentiate = TRUE, parametric = TRUE) 
modSens1_Results <- tidy(modSens1, exponentiate = TRUE, parametric = TRUE) 
modSens2_Results <- tidy(modSens2, exponentiate = TRUE, parametric = TRUE) 
modSens3_Results <- tidy(modSens3, exponentiate = TRUE, parametric = TRUE) 


sens_Results <- mod_Results %>% 
  filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
  bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                       estimate = rep(1, 4),
                       std.error = rep(NA_real_, 4),
                       statistics = rep(NA_real_, 4),
                       p.value = rep(NA_real_, 4))) %>% 
  mutate(mod = 'mod') %>% 
  bind_rows(modSens1_Results %>% 
              filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
              bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                                   estimate = rep(1, 4),
                                   std.error = rep(NA_real_, 4),
                                   statistics = rep(NA_real_, 4),
                                   p.value = rep(NA_real_, 4))) %>% 
              mutate(mod = 'modSens1')) %>% 
  bind_rows(modSens2_Results %>% 
              filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
              bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                                   estimate = rep(1, 4),
                                   std.error = rep(NA_real_, 4),
                                   statistics = rep(NA_real_, 4),
                                   p.value = rep(NA_real_, 4))) %>% 
              mutate(mod = 'modSens2')) %>% 
  bind_rows(modSens3_Results %>% 
              filter(substr(term, 1, 3) %in% c('imd', 'sex', 'eth', 'icb')) %>% 
              bind_rows(data.frame(term = c('imdQF1 - most deprived (ref)', 'sexMfmale (ref)', 'ethnicityWhite - British (ref)', 'icbShortNameCambridgeshire & Peterborough (ref)' ),
                                   estimate = rep(1, 4),
                                   std.error = rep(NA_real_, 4),
                                   statistics = rep(NA_real_, 4),
                                   p.value = rep(NA_real_, 4))) %>% 
              mutate(mod = 'modSens3')) %>% 
  mutate(lcl95 = case_when(mod == 'mod' ~ estimate - 1.96*std.error,
                           TRUE ~ NA_real_),
         ucl95 = case_when(mod == 'mod' ~ estimate + 1.96*std.error,
                           TRUE ~ NA_real_)) %>% 
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

# 6 create chart outputs for sensitivity analysis----
sens_Results %>% 
  filter(termGrp == 'IMD') %>% 
  ggplot() +
  geom_vline(aes(xintercept = 1), colour = 'grey', linetype = 'dashed') +
  geom_segment(aes(y = termLabel, yend = termLabel, x = ucl95, xend = lcl95)) +
  geom_point(aes(y = termLabel, x = estimate, colour = mod, size = mod)) +
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
  geom_point(aes(y = termLabel, x = estimate, colour = mod, size = mod)) +
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
  geom_point(aes(y = termLabel, x = estimate, colour = mod, size = mod)) +
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
  geom_point(aes(y = termLabel, x = estimate, colour = mod, size = mod)) +
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