library(tidyverse)
library(lubridate)
library(scales)
library(here)
library(maps) # not sure if this is needed
library(mapdata) # not sure if this is needed
library(maptools) 
library(rgdal)
library(ggmap) # not sure if this is needed
library(rgeos)
library(broom)
library(ggpubr)




commDataSubmissionCounts <- read_csv(here('dataRaw', 'commDataSubmissionCounts.csv')) %>% 
  mutate(monthYear = as_date(monthYear))

saveRDS(commDataSubmissionCounts, here('dataRDS', 'commDataSubmissionCounts.RDS'))

plotProvidersSubmitting <- commDataSubmissionCounts %>% 
  pivot_longer(cols = 2:5, names_to = 'metric', values_to = 'value') %>% 
  filter(metric == 'contact data') %>% v
  ggplot() +
  geom_line(aes(x = monthYear, y = value),
             colour = '#f9bf07') +
  geom_point(aes(x = monthYear, y = value),
             colour = '#5881c1') +
  scale_y_continuous(name = '', limits = c(0, NA_real_)) +
  scale_x_date(name = '') +
  labs(title = 'Number of providers submitting contact data',
       subtitle = 'England | October 2017 - October 2022')
  
plotContactsSubmitted <- commDataSubmissionCounts %>% 
  pivot_longer(cols = 2:5, names_to = 'metric', values_to = 'value') %>% 
  filter(metric == 'contacts') %>% 
  ggplot() +
  geom_line(aes(x = monthYear, y = value),
            colour = '#f9bf07') +
  geom_point(aes(x = monthYear, y = value),
             colour = '#5881c1') +
  scale_y_continuous(name = '', 
                     limits = c(0, NA_real_),
                     label = comma_format()) +
  scale_x_date(name = '') +
  labs(title = 'Community contacts submited',
       subtitle = 'England | October 2017 - October 2022')

ggarrange(plotProvidersSubmitting, plotContactsSubmitted, ncol = 2, nrow = 1)

ggsave(file = here('charts', 'commDataSubmissions.png'),
       plot = last_plot(),
       device = 'jpg',
       width = 29.7,
       height = 14,
       units = 'cm',
       dpi = 700)



commContactsLsoa <- readRDS(here('dataRDS', 'commContactsLsoa.RDS'))

icbBoundaries <- readOGR(here('dataRaw', 'Integrated_Care_Boards_(July_2022)_EN_BGC.shp')) %>% 
  tidy(region = 'ICB22CD') 

lsoaBoundaries <- readOGR(here('dataRaw', 'Lower_Layer_Super_Output_Areas_(Dec_2011)_Boundaries_Full_Clipped_(BFC)_EW_V3.shp')) %>% 
  tidy(region = 'LSOA11CD')
  
commContactsLsoaDf <- commContactsLsoa %>% 
  filter(substr(LSOA, 1, 1) == 'E') %>% 
  group_by(LSOA) %>% 
  summarise(contactAll = sum(contactall, na.rm = TRUE),
            contactAttended = sum(contactAttended, na.rm = TRUE),
            contactAttendedF2F = sum(contactAttendedF2F, na.rm = TRUE),
            contactF2F = sum(contactF2F, na.rm = TRUE),
            contactAttendOrNotInvalid = sum(contactAttenOrNorInvalid, na.rm = TRUE),
            contactConsMediumInvalid = sum(contactConsMediumInvalid, na.rm = TRUE)) %>% 
  mutate(pContactAttendOrNotInvalid = contactAttendOrNotInvalid / contactAll,
         pContactConsMediumInvalid = contactConsMediumInvalid / contactAll) %>% 
  pivot_longer(cols = 2:9, names_to = 'metric', values_to = 'value') %>% 
  mutate(value = ifelse(metric %in% c('contactAll', 'contactAttended', 
                                      'contactAttendedF2F', 'contactF2F') & value == 0, NA_real_, value)) %>% 
  group_by(metric) %>% 
  mutate(ntile9 = ntile(value, 9),
         equGrp9 = case_when(metric %in% c('contactAll', 'contactAttended', 
                                           'contactAttendedF2F', 'contactF2F') ~ NA_real_,
                             TRUE ~ floor(value/(0.112))))
  # summarise(min = min(value),
  #           lq = quantile(value, 0.25),
  #           median = quantile(value, 0.5),
  #           uq = quantile(value, 0.75),
  #           max = max(value),
  #           mean = mean(value))
  

lsoaDf <- lsoaBoundaries %>% 
  filter(substr(id, 1, 1) == 'E') %>% 
  left_join((commContactsLsoaDf %>% 
               filter(metric == 'contactAll')), 
            by = c('id' = 'LSOA')) %>% 
  mutate(contactAllN9 = ntile9) %>% 
  select(-ntile9, -equGrp9) %>% 
  left_join((commContactsLsoaDf %>% 
               filter(metric == 'contactAttended')), 
            by = c('id' = 'LSOA')) %>% 
  mutate(contactAttendedN9 = ntile9) %>% 
  select(-ntile9, -equGrp9) %>% 
  left_join((commContactsLsoaDf %>% 
               filter(metric == 'contactAttendedF2F')), 
            by = c('id' = 'LSOA')) %>% 
  mutate(contactAttendedF2FN9 = ntile9) %>% 
  select(-ntile9, -equGrp9) %>% 
  left_join((commContactsLsoaDf %>% 
               filter(metric == 'pContactAttendOrNotInvalid')), 
            by = c('id' = 'LSOA')) %>% 
  mutate(contactAttendedInvalidG9 = equGrp9) %>% 
  select(-ntile9, -equGrp9) %>% 
  left_join((commContactsLsoaDf %>% 
               filter(metric == 'pContactConsMediumInvalid')), 
            by = c('id' = 'LSOA')) %>% 
  mutate(contactMediumInvaidG9 = equGrp9) %>% 
  select(-ntile9, -equGrp9)
  

plotContactAllN9 <- ggplot() +
  geom_polygon(data = lsoaDf, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = as.factor(contactAllN9)),
               colour = NA) +
  geom_polygon(data = icbBoundaries, 
               aes(x = long, y = lat, 
                   group = group), 
               colour = 'black', 
               fill = NA,
               linewidth = 0.5) +  
  scale_fill_brewer(palette = 'YlOrRd', na.value = 'blue') +
  coord_equal() +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  labs(title = 'Contact appointments')


plotContactAttN9 <- ggplot() +
  geom_polygon(data = lsoaDf, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = as.factor(contactAttendedN9)),
               colour = NA) +
  geom_polygon(data = icbBoundaries, 
               aes(x = long, y = lat, 
                   group = group), 
               colour = 'black', 
               fill = NA,
               linewidth = 0.5) +  
  scale_fill_brewer(palette = 'YlOrRd', na.value = 'blue') +
  coord_equal() +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  labs(title = 'Contacts attended')


plotContactAttF2FN9 <- ggplot() +
  geom_polygon(data = lsoaDf, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = as.factor(contactAttendedF2FN9)),
               colour = NA) +
  geom_polygon(data = icbBoundaries, 
               aes(x = long, y = lat, 
                   group = group), 
               colour = 'black', 
               fill = NA,
               linewidth = 0.5) +  
  scale_fill_brewer(palette = 'YlOrRd', na.value = 'blue') +
  coord_equal() +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  labs(title = 'Contacts attended F2F')




ggarrange(plotContactAllD, plotContactAttD, plotContactAttF2FD, ncol = 3, nrow = 1)

ggsave(file = here('charts', 'lsoaCoverage.png'),
       plot = last_plot(),
       device = 'jpg',
       width = 29.7,
       height = 21,
       units = 'cm',
       dpi = 700)




plotContactAttendedInvalidG9 <- ggplot() +
  geom_polygon(data = lsoaDf, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = as.factor(contactAttendedInvalidG9)),
               colour = NA) +
  geom_polygon(data = icbBoundaries, 
               aes(x = long, y = lat, 
                   group = group), 
               colour = 'black', 
               fill = NA,
               linewidth = 0.5) +  
  scale_fill_brewer(palette = 'YlOrRd', na.value = 'blue') +
  coord_equal() +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  labs(title = '% contacts with invalid attendance code')


plotContactMediumInvaidG9 <- ggplot() +
  geom_polygon(data = lsoaDf, 
               aes(x = long, y = lat, 
                   group = group, 
                   fill = as.factor(contactMediumInvaidG9)),
               colour = NA) +
  geom_polygon(data = icbBoundaries, 
               aes(x = long, y = lat, 
                   group = group), 
               colour = 'black', 
               fill = NA,
               linewidth = 0.5) +  
  scale_fill_brewer(palette = 'YlOrRd', na.value = 'blue') +
  coord_equal() +
  theme_minimal() +
  theme(axis.title.y = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(), 
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        legend.position = 'none') +
  labs(title = '% contacts with invalid consultation medium')



ggarrange(plotContactAttendedInvalidG9, plotContactMediumInvaidG9, ncol = 3, nrow = 1)

ggsave(file = here('charts', 'lsoaCoverageInvalid.png'),
       plot = last_plot(),
       device = 'jpg',
       width = 29.7,
       height = 21,
       units = 'cm',
       dpi = 700)





commContactsLsoa %>% 
  summarise(contactAll = sum(contactall, na.rm = TRUE),
            contactAttended = sum(contactAttended, na.rm = TRUE),
            contactAttendedF2F = sum(contactAttendedF2F, na.rm = TRUE),
            contactF2F = sum(contactF2F, na.rm = TRUE),
            contactAttendOrNotInvalid = sum(contactAttenOrNorInvalid, na.rm = TRUE),
            contactConsMediumInvalid = sum(contactConsMediumInvalid, na.rm = TRUE)) %>% 
  mutate(pContactAttendOrNotInvalid = contactAttendOrNotInvalid / contactAll,
         pContactConsMediumInvalid = contactConsMediumInvalid / contactAll)
