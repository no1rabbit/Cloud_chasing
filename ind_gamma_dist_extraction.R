#setting up environment
library(tidyverse)
library(amt)
library(lubridate)
library(vistime)
library(sf)
library(terra)
library(readr)
library(beepr)

#read in data
#data <- read_csv('data/Serengeti_all.csv')
data <- read_csv('data/serengeti_wb_HMM.csv')
#data <- data %>%
 # mutate(time_diff = abs(as.numeric(difftime(hour(timestamp), 7, units = "hours"))),
 #        date = date(timestamp)) %>%
#  group_by(ID,date) %>% 
#  filter(time_diff == min(time_diff))


#data <- data %>% filter(timestamp > as.Date('2015-01-01')) %>% filter(!ID %in% c('SW76', 'ST5H-5598')) 
#data$timestamp_hour <- round_date(data$timestamp, "hour")
# data <- data %>% group_by(ID) %>% 
#   distinct(timestamp_hour, .keep_all = T) %>% 
#   mutate(migrant = recode(migrant,'r' = 'resident','m' = 'migrant')) %>% 
#   ungroup()

#for all individuals
data <- data %>% select(ID,x1_,y1_,species,migrant,date,t1_,x,y,sex, dailystate, year,months,mon2,season) %>% nest(data = c(-ID))

data <- data %>% 
  mutate(trk = map(.x = data, function(d) {
    amt::make_track(d, x1_, y1_, t1_, crs = 21036, all_cols = T) 
  }),
  trk = map(trk, ~ track_resample(., rate = days(1), tolerance = minutes(30))))
#trk = map(trk, ~ track_resample(., rate = hours(12), tolerance = minutes(20))))


## remove individuals that have less than 2 months of data

data <- data %>% select(ID, trk) %>% unnest(trk) %>% 
  group_by(ID) %>% 
  filter(n() >= 90) 
class(data) <- c("track_xyt","track_xy","tbl_df","tbl","data.frame", class(data))


data <- data %>% 
  nest(trk=c(-ID))
  

data %>% mutate(sr = lapply(trk, summarize_sampling_rate)) %>%
  amt::select(ID, sr) %>% unnest(cols = sr) %>% print(n=250)

data <- data %>% 
  mutate(stps = map(trk, ~ steps_by_burst(., keep_cols = 'start')))#,
         #stps = map(stps, ~ add_nsd(.)))  

set.seed(1234)
data <- data %>% 
  mutate(ssf = lapply(stps, function(x){
    x %>% random_steps() %>%
      mutate(cos_ta_ = cos(ta_),
             log_sl_ = log(sl_))}))

data <- data %>% mutate(ssf = map(ssf,remove_incomplete_strata))

data <- data %>% mutate(sl_dist = map(ssf,sl_distr_params),
                        ta_dist = map(ssf,ta_distr_params))
#data
#beep(5)
m<-data %>% select(ID,ssf,sl_dist,ta_dist) %>% 
  unnest_wider(c(sl_dist,ta_dist),names_sep = '_') %>% 
  unnest(cols = ssf)

write_csv(m, 'data/Serengeti_HMM.csv',na = '')

class(m)
class(m) <- c("random_steps","bursted_steps_xyt","steps_xyt","steps_xy", class(m))

m <- m %>% nest(ssf = c(-ID,-site)) 
m %>% mutate(ssf = map(ssf,plot_sl))
saveRDS(m,'RDS_files/ssf_1day_14-06-2023.rds')

m1 %>% group_by(site) %>% 
  summarise(`no. of individuals` = n_distinct(ID))

