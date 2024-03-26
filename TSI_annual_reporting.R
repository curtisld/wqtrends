library(tidyverse)
library(lubridate)

load("C:/Users/degaspec/OneDrive - King County/R/wqtrends/data/datprc.RData")

unique(datprc$station)
unique(datprc$param)

tsi <- filter(datprc, station %in% c('0852','0612','A522')&param == 'TSI_chla') %>% mutate(Year = year(date), Month = month(date)) %>% 
                    filter(between(Month,6,9)) %>% group_by(Year,station) %>% 
                       summarize(Value = round(mean(value),1)) %>% ungroup() %>% spread(key=station, value = Value)


write_csv(tsi,'tsi_report.csv')
