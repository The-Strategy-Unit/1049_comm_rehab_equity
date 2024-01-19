# create charts for MPFT MSk away day

library(tidyverse)
library(here)
library(broom)
library(mgcv)

# read in dataframes abd relevel at IMDQ 3
modDf_All_mpft <- readRDS(here('dataRDS', 'modDf.RDS')) |> 
  mutate(imdQF = relevel(imdQF, ref = '3'))
  
modDf_fracFemur_mpft <- readRDS(here('dataRDS', 'modDf_fracFemur.RDS')) |> 
  mutate(imdQF = relevel(imdQF, ref = '3'))


# build and save models and coeffcients
mod_All_mpft <- bam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicity + icbShortName + 
             specGrp + podGrp + 
             s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
             s(der_spell_LoS, bs = 'cr') + 
             s(cci, bs = 'cr') + 
             s(priorCommContactsAttF2F, bs = 'cr'),
           data = modDf_All_mpft,
           family = binomial(link = "logit"))

saveRDS(mod_All_mpft, here('dataRDS', 'mod_All_mpft.RDS'))

mod_All_mpft_Results <- tidy(mod_All_mpft, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_All_mpft_Results, here('dataRDS', 'mod_All_mpft_Results.RDS'))
# mod_All_mpft_Results <- readRDS(here('dataRDS', 'mod_All_mpft_Results.RDS'))



mod_fracFemur_mpft <- gam(postCommContactAttF2FYN ~ imdQF +  sexMf + ethnicityGrp + 
                       s(Der_Age_at_CDS_Activity_Date, bs = 'cr') + 
                       s(der_spell_LoS, bs = 'cr') + 
                       s(cci, bs = 'cr') + 
                       s(priorCommContactsAttF2F, bs = 'cr'),
                     data = modDf_fracFemur_mpft,
                     family = binomial(link = "logit"))



saveRDS(mod_fracFemur_mpft, here('dataRDS', 'mod_fracFemur_mpft.RDS'))

mod_fracFemur_mpft_Results <- tidy(mod_fracFemur_mpft, exponentiate = FALSE, parametric = TRUE) 

saveRDS(mod_fracFemur_mpft_Results, here('dataRDS', 'mod_fracFemur_mpft_Results.RDS'))
# mod_fracFemur_mpft_Results <- readRDS(here('dataRDS', 'mod_fracFemur_mpft_Results.RDS'))


# Visualise results

mod_All_mpft_Results |> 
  filter(substr(term, 1, 3) == 'imd') |> 
  bind_rows(data.frame(term = 'imdQF3',
                       estimate = 0,
                       std.error = NA_real_,
                       statistics = NA_real_,
                       p.value = NA_real_)) |>  
  mutate(or = exp(estimate),
         lcl95 = exp(estimate - 1.96*std.error),
         ucl95 = exp(estimate + 1.96*std.error)) |> 
  mutate(patGrp = 'all') |> 
  bind_rows(mod_fracFemur_mpft_Results |> 
              filter(substr(term, 1, 3) == 'imd') |> 
              bind_rows(data.frame(term = 'imdQF3',
                                   estimate = 0,
                                   std.error = NA_real_,
                                   statistics = NA_real_,
                                   p.value = NA_real_)) |> 
              mutate(or = exp(estimate),
                     lcl95 = exp(estimate - 1.96*std.error),
                     ucl95 = exp(estimate + 1.96*std.error)) |> 
              mutate(patGrp = 'fnof')) |> 
  mutate(patGrpLabel = ifelse(patGrp == 'all', 'all patients', 'fractured neck of femur')) |> 
  mutate(termLabel = ifelse(term == 'imdQF1', '1 - most deprived', substr(term, 6, 6))) |> 
  ggplot() +
  geom_smooth(aes(x = as.numeric(substr(termLabel, 1, 1)), 
                  y = or),
              method = 'lm',
              formula = 'y~x',
              colour = 'red') +
  geom_segment(aes(x = termLabel, xend = termLabel,
                   y = ucl95, yend = lcl95)) +
  geom_point(aes(x = termLabel, y = or),
             colour = 'red') +
  facet_wrap(~patGrpLabel) +
  scale_y_log10(name = 'adjusted odds ratio (log scale)',
                breaks = c(0.8, 0.9, 1.0, 1.1, 1.2, 1.3)) +
  scale_x_discrete(name = '') +
  labs(title = 'Impact of deprivation on post-discharge community services contact',
       subtitle = 'England | June & July 2022',
       caption = 'whiskers denote 95% confidence intervals')

ggsave(here('charts', 'plotMpftORs.jpg'), 
       device = 'jpg',
       height = 10,
       width = 20,
       units = 'cm',
       dpi = 300)


modDf_All_mpft |> 
  group_by(postCommContactAttF2FYN) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n))

modDf_fracFemur_mpft |> 
  group_by(postCommContactAttF2FYN) |> 
  summarise(n = n()) |> 
  mutate(p = n / sum(n))
