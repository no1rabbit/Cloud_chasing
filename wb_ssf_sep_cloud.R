#'---
#'title: SSF analysis of wildebeest 
#'author: "M. Boyers"
#'date: "`r format(Sys.time(), '%d %B, %Y')`"
#'output:
#'  html_document:
#'    toc: yes
#'---

#+ include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, cache = TRUE)


#' ## Load libraries and prepare data
#+ echo=TRUE, message=FALSE, warning=FALSE
#library(glmmTMB)
library(tidyverse)
library(raster)
library(survival)
library(amt)
library(sjPlot)
library(readr)



data <- read_csv('data/Serengeti_data2.csv')

data<- data %>% filter(case_ == TRUE) %>% select(date1,ID,site,sl_,ta_,cloud_5mm_rfe,cloud_angle_radians_5mm,magnitude_cloud_5mm,cloud_10mm_rfe,cloud_angle_radians_10mm,magnitude_cloud_10mm,cloud_15mm_rfe,cloud_angle_radians_15mm,magnitude_cloud_15mm) %>% 
  mutate(sl_ = sl_/1000,
         magnitude_cloud_5mm = magnitude_cloud_5mm/1000,
         magnitude_cloud_10mm = magnitude_cloud_10mm/1000,
         magnitude_cloud_15mm = magnitude_cloud_15mm/1000)

data <- data %>% 
  mutate(months = month(date1,label=T,abbr=T),
         mon2 = case_when(
           months == 'Oct'  | months == 'Nov' ~ "Oct-Nov",
           months == 'Dec' | months == 'Jan' ~ "Dec-Jan",
           months == 'Feb' | months == 'Mar' ~ "Feb-Mar",
           months == 'Apr' | months == 'May' ~ "Apr-May",
           months == 'Jun' | months == 'Jul' ~ "Jun-Jul",
           months == 'Aug' | months == 'Sep' ~ "Aug-Sep"
         ),
         cloud_direction = if_else(0<cos(cloud_angle_radians_5mm)<1))

data %>% 
  ggplot(aes(months,cos(cloud_angle_radians_5mm)))+
  geom_boxplot()

data %>% 
  ggplot(aes(months,cos(cloud_angle_radians_10mm)))+
  geom_boxplot()

data %>% 
  ggplot(aes(months,cos(cloud_angle_radians_15mm)))+
  geom_boxplot()

data %>% 
  ggplot(aes(cos(cloud_angle_radians_5mm),magnitude_cloud_5mm))+
  geom_point()+
  facet_wrap(~months)

class(data)
class(data) <- c("random_steps","bursted_steps_xyt","steps_xyt","steps_xy", class(data))

summary(data)
data1 <- data %>% select(ID,site,case_,sl_,ta_,step_id_,cos_ta_,log_sl_,sl_dist_scale,sl_dist_shape,ta_dist_kappa,NDVI_end, NDVI_start,dNDVI_end, dNDVI_start,distance_to_tamsat_raincloud,
                          angle_to_tamsat_raincloud,
                          turning_angle_difference_tamsat_raincloud,
                          distance_to_raincloud,
                          angle_to_raincloud,
                          turning_angle_difference_chirps)

data1 <- data1 %>% remove_incomplete_strata(col = 'NDVI_end')
summary(data1)

data1 <- data1 %>%
  nest(stps = c(-ID, -site)) %>%
  mutate(stps = map(stps, ~ {
    shape <- .x$sl_dist_shape[1]
    scale <- .x$sl_dist_scale[1]
    attributes(.x)$sl_ <- make_gamma_distr(shape=shape,scale = scale)
    kappa <- .x$ta_dist_kappa[1]
    attributes(.x)$ta_ <- make_vonmises_distr(kappa = kappa)
    return(.x)
  }))

data1 <- data1 %>% mutate(fit_no_co = map(stps, ~ fit_issf(case_ ~ #NDVI_end + 
                #distance_to_tamsat_raincloud + 
                #angle_to_tamsat_raincloud +
                #turning_angle_difference_tamsat_raincloud +
                #distance_to_raincloud +
                #angle_to_raincloud +
                #turning_angle_difference_chirps +
                #distance_to_sw + 
                sl_ + log_sl_ + cos_ta_ + 
                strata(step_id_), data=., model=T)),
                fit_co_ndvi = map(stps, ~ fit_issf(case_ ~ NDVI_end + 
                                                   #distance_to_tamsat_raincloud + 
                                                   #angle_to_tamsat_raincloud +
                                                   #turning_angle_difference_tamsat_raincloud +
                                                   #distance_to_raincloud +
                                                   #angle_to_raincloud +
                                                   #turning_angle_difference_chirps +
                                                   #distance_to_sw + 
                                                   sl_ + log_sl_ + cos_ta_ + 
                                                   strata(step_id_), data=., model=T)),
                fit_co_ndvi_tamsat = map(stps, ~ fit_issf(case_ ~ NDVI_end + 
                                                            distance_to_tamsat_raincloud + 
                                                            angle_to_tamsat_raincloud +
                                                            turning_angle_difference_tamsat_raincloud +
                                                            #distance_to_raincloud +
                                                            #angle_to_raincloud +
                                                            #turning_angle_difference_chirps +
                                                            #distance_to_sw + 
                                                            sl_ + log_sl_ + cos_ta_ + 
                                                            strata(step_id_), data=., model=T)),
                fit_co_tamsat = map(stps, ~ fit_issf(case_ ~ #NDVI_end + 
                                                            distance_to_tamsat_raincloud + 
                                                            angle_to_tamsat_raincloud +
                                                            cos(turning_angle_difference_tamsat_raincloud) +
                                                            #distance_to_raincloud +
                                                            #angle_to_raincloud +
                                                            #turning_angle_difference_chirps +
                                                            #distance_to_sw + 
                                                            sl_ + log_sl_ + cos_ta_ + 
                                                            strata(step_id_), data=., model=T)),
                fit_co_ndvi_chirps = map(stps, ~ fit_issf(case_ ~ NDVI_end + 
                                                            #distance_to_tamsat_raincloud + 
                                                            #angle_to_tamsat_raincloud +
                                                            #turning_angle_difference_tamsat_raincloud +
                                                            distance_to_raincloud +
                                                            angle_to_raincloud +
                                                            turning_angle_difference_chirps +
                                                            #distance_to_sw + 
                                                            sl_ + log_sl_ + cos_ta_ + 
                                                            strata(step_id_), data=., model=T)),
                fit_co_ndvi_chirps_sw = map(stps, ~ fit_issf(case_ ~ NDVI_end + 
                                                            #distance_to_tamsat_raincloud + 
                                                            #angle_to_tamsat_raincloud +
                                                            #turning_angle_difference_tamsat_raincloud +
                                                            distance_to_raincloud +
                                                            angle_to_raincloud +
                                                            turning_angle_difference_chirps +
                                                            #distance_to_sw + 
                                                            sl_ + log_sl_ + cos_ta_ + 
                                                            strata(step_id_), data=., model=T))) 

data1 %>% mutate(coef = map(fit_co_tamsat, ~ broom::tidy(.x$model,conf.int = T, conf.level = 0.95))) %>%
  dplyr::select(ID, site, coef) %>% unnest(cols = c(coef)) %>% 
  ggplot(., aes(x = term, y = estimate, group = ID, col = site)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "movement metrics", y = "Relative Selection Strength") +
  theme_light() +
  #scale_y_continuous(limits = c(-2, 2))+
  scale_x_discrete(labels = c("distance_to_tamsat_raincloud", "angle_to_tamsat_raincloud", "cos(turning_angle_difference_tamsat_raincloud)","sl_", "log_sl_","turning angle"))

data1 %>% amt::select(ID,site,fit_no_co) %>% 
  mutate(sl_before = map(fit_no_co, sl_distr),
         sl_update = map(fit_no_co, update_sl_distr)) %>% 
  amt::select(ID,site,sl_before,sl_update) %>% 
  unnest_wider(c(sl_before,sl_update),names_sep = '_') %>% 
  amt::select(ID,site,sl_before_params,sl_update_params) %>% 
  unnest_wider(c(sl_before_params,sl_update_params),names_sep = '_') %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

global_plot_sl<- dat_ssf_global %>% amt::select(ID,site,fit) %>% 
  mutate(sl_distrb = map(fit, sl_distr),
         sl_update = map(fit, update_sl_distr)) %>% 
  amt::select(ID,sl_distrb,sl_update) %>% 
  mutate(x = map(ID,function(x) {x = seq(from =0, to =1000, length.out =100)}),
         global = map(sl_distrb,~dgamma(x = seq(from =0, to =1000, length.out =100), shape = .x$params$shape, scale = .x$params$scale)),
         updated = map(sl_update,~dgamma(x = seq(from =0, to =1000, length.out =100), shape = .x$params$shape, scale = .x$params$scale))) %>%
  dplyr::select(ID, x,global,updated) %>% 
  unnest(cols = c(ID,x,global,updated)) %>% 
  pivot_longer(cols = -c(ID,x))

ggplot(global_plot_sl, aes(x = x, y = value, color = ID)) + 
  geom_line(size = 1) + 
  xlab("Step Length (m)") + 
  ylab("Probability Density") +
  facet_wrap(~name)+
  guides(colour=FALSE)+
  theme_bw()

data %>% amt::select(ID,site,fit_no_co) %>% 
  mutate(ta_distrb = map(fit_no_co, ta_distr),
         ta_update = map(fit_no_co, update_ta_distr)) %>% 
  amt::select(ID,site,ta_distrb,ta_update) %>% 
  unnest_wider(c(ta_distrb,ta_update),names_sep = '_') %>% 
  amt::select(ID,site,ta_distrb_params,ta_update_params) %>% 
  unnest_wider(c(ta_distrb_params,ta_update_params),names_sep = '_') %>%
  amt::select(ID,site,ta_distrb_params_kappa,ta_update_params_kappa) %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

data %>% amt::select(ID,site,fit_co_ndvi) %>% 
  mutate(sl_before = map(fit_co_ndvi, sl_distr),
         sl_update = map(fit_co_ndvi, update_sl_distr)) %>% 
  amt::select(ID,site,sl_before,sl_update) %>% 
  unnest_wider(c(sl_before,sl_update),names_sep = '_') %>% 
  amt::select(ID,site,sl_before_params,sl_update_params) %>% 
  unnest_wider(c(sl_before_params,sl_update_params),names_sep = '_') %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

data %>% amt::select(ID,site,fit_co_ndvi) %>% 
  mutate(ta_distrb = map(fit_co_ndvi, ta_distr),
         ta_update = map(fit_co_ndvi, update_ta_distr)) %>% 
  amt::select(ID,site,ta_distrb,ta_update) %>% 
  unnest_wider(c(ta_distrb,ta_update),names_sep = '_') %>% 
  amt::select(ID,site,ta_distrb_params,ta_update_params) %>% 
  unnest_wider(c(ta_distrb_params,ta_update_params),names_sep = '_') %>%
  amt::select(ID,site,ta_distrb_params_kappa,ta_update_params_kappa) %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

data %>% amt::select(ID,site,fit_co_ndvi_tamsat) %>% 
  mutate(sl_before = map(fit_co_ndvi_tamsat, sl_distr),
         sl_update = map(fit_co_ndvi_tamsat, update_sl_distr)) %>% 
  amt::select(ID,site,sl_before,sl_update) %>% 
  unnest_wider(c(sl_before,sl_update),names_sep = '_') %>% 
  amt::select(ID,site,sl_before_params,sl_update_params) %>% 
  unnest_wider(c(sl_before_params,sl_update_params),names_sep = '_') %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

data %>% amt::select(ID,site,fit_co_ndvi_tamsat) %>% 
  mutate(ta_distrb = map(fit_co_ndvi_tamsat, ta_distr),
         ta_update = map(fit_co_ndvi_tamsat, update_ta_distr)) %>% 
  amt::select(ID,site,ta_distrb,ta_update) %>% 
  unnest_wider(c(ta_distrb,ta_update),names_sep = '_') %>% 
  amt::select(ID,site,ta_distrb_params,ta_update_params) %>% 
  unnest_wider(c(ta_distrb_params,ta_update_params),names_sep = '_') %>%
  amt::select(ID,site,ta_distrb_params_kappa,ta_update_params_kappa) %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

data1 %>% amt::select(ID,site,fit_co_tamsat) %>% 
  mutate(sl_before = map(fit_co_tamsat, sl_distr),
         sl_update = map(fit_co_tamsat, update_sl_distr)) %>% 
  amt::select(ID,site,sl_before,sl_update) %>% 
  unnest_wider(c(sl_before,sl_update),names_sep = '_') %>% 
  amt::select(ID,site,sl_before_params,sl_update_params) %>% 
  unnest_wider(c(sl_before_params,sl_update_params),names_sep = '_') %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')

data %>% amt::select(ID,site,fit_co_tamsat) %>% 
  mutate(ta_distrb = map(fit_co_tamsat, ta_distr),
         ta_update = map(fit_co_tamsat, update_ta_distr)) %>% 
  amt::select(ID,site,ta_distrb,ta_update) %>% 
  unnest_wider(c(ta_distrb,ta_update),names_sep = '_') %>% 
  amt::select(ID,site,ta_distrb_params,ta_update_params) %>% 
  unnest_wider(c(ta_distrb_params,ta_update_params),names_sep = '_') %>%
  amt::select(ID,site,ta_distrb_params_kappa,ta_update_params_kappa) %>% 
  pivot_longer(cols = -c(ID,site),cols_vary = 'slowest',names_to = c('type','params.id','x','params'),names_sep = '_') %>% 
  ggplot(.,aes(site,value,col = ID, pch=site)) +
  geom_point(position = position_dodge(width = 0.7))+
  guides(colour=FALSE)+
  facet_wrap(params.id~params, scale='free')
