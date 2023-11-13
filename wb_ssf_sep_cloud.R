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
data <- read_csv('data/Serengeti_all.csv')

data<- data %>% #filter(case_ == TRUE) %>% 
  select(date1,ID,site,case_,sl_,step_id_,sl_dist_shape,sl_dist_scale,cloud_5mm_rfe,cloud_angle_radians_5mm,magnitude_cloud_5mm,cloud_10mm_rfe,cloud_angle_radians_10mm,magnitude_cloud_10mm,cloud_15mm_rfe,cloud_angle_radians_15mm,magnitude_cloud_15mm) %>% 
  mutate(sl_ = sl_/1000,
         magnitude_cloud_5mm = magnitude_cloud_5mm/1000,
         magnitude_cloud_10mm = magnitude_cloud_10mm/1000,
         magnitude_cloud_15mm = magnitude_cloud_15mm/1000,
         log_sl_ = log(sl_),
         cloud_angle_radians_5mm = cos(cloud_angle_radians_5mm),
         cloud_angle_radians_10mm = cos(cloud_angle_radians_10mm)) %>% 
  rename(
    c_5mm = cloud_5mm_rfe,
    ca_5 = cloud_angle_radians_5mm,
    cd_5 = magnitude_cloud_5mm,
    c_10mm = cloud_10mm_rfe,
    ca_10 = cloud_angle_radians_10mm,
    cd_10 = magnitude_cloud_10mm,
    c_15mm = cloud_15mm_rfe,
    ca_15 = cloud_angle_radians_15mm,
    cd_15 = magnitude_cloud_15mm
  ) #%>% 
  # mutate(c_5mm = ifelse(!is.na(c_5mm) & (c_5mm >= 5 & c_5mm < 10), c_5mm, NA),
  #        ca_5 = ifelse(is.na(c_5mm), NA, ca_5),
  #        cd_5 = ifelse(is.na(c_5mm), NA, cd_5),
  #        c_10mm = ifelse(!is.na(c_10mm) & (c_10mm >= 10 & c_10mm < 15), c_10mm, NA),
  #        ca_10 = ifelse(is.na(c_10mm), NA, ca_10),
  #        cd_10 = ifelse(is.na(c_10mm), NA, cd_10)
  #        )

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
         season = case_when(
           months %in% c('Nov','Dec','Jan') ~ 'Dry to Wet transition',
           months %in% c('Feb','Mar','Apr') ~ 'Wet season',
           months %in% c('May', 'Jun','Jul') ~ 'Wet to Dry transition',
           TRUE ~ 'Dry season'
         ),
         angle_cat_5 = case_when(
           ca_5 >= 0.5 & ca_5 < 1 ~ "possibly chasing clouds",
           ca_5 > 0 & ca_5 < 0.5 ~ "maybe chasing clouds",
           ca_5 > -1 & ca_5 <= 0 ~ "not chasing clouds",
           ca_5 == 1 ~ "chasing clouds",
           ca_5 == -1 ~ "opp direction of cloud",
           TRUE ~ "no clouds"  # Add this line to handle other cases if needed
           ),
         angle_cat_10 = case_when(
           ca_10 >= 0.5 & ca_10 < 1 ~ "possibly chasing clouds",
           ca_10 > 0 & ca_10 < 0.5 ~ "maybe chasing clouds",
           ca_10 > -1 & ca_10 < 0 ~ "not chasing clouds",
           ca_10 == 1 ~ "chasing clouds",
           ca_10 == -1 ~ "opp direction of cloud",
           TRUE ~ "no clouds"  # Add this line to handle other cases if needed
         ))


# Suggested 2 month blocks: 
#   
# Oct-Nov –> dry-wet transition
# Dec-Jan –> short wet
# Feb-March –> calving and new borns
# April-May –> core wet
# June-July –> wet-dry transition
# Aug-Sept –> core dry


data %>% filter(case_ == TRUE & angle_cat_5 != 'no clouds') %>% 
  ggplot(aes(months,ca_5))+
  geom_boxplot()

data %>% filter(case_ == TRUE & angle_cat_5 != 'no clouds') %>% 
  ggplot(aes(months,fill = angle_cat_5))+
  geom_bar(position = "dodge")

data %>% filter(case_ == TRUE & angle_cat_5 != 'no clouds') %>% 
  ggplot(aes(c_5mm, cd_5, colour=angle_cat_5))+
  geom_point()


data %>% filter(case_ == TRUE & angle_cat_10 != 'no clouds') %>% 
  ggplot(aes(months,ca_10))+
  geom_boxplot()

data %>% filter(case_ == TRUE & !is.na(ca_15)) %>% 
  ggplot(aes(months,ca_15))+
  geom_boxplot()

data %>% filter(case_ == TRUE) %>% 
  ggplot(aes(cos(cloud_angle_radians_5mm),magnitude_cloud_5mm))+
  geom_point()+
  facet_wrap(~months)

class(data1)
class(data) <- c("random_steps","bursted_steps_xyt","steps_xyt","steps_xy", class(data))

summary(data)

data1 <- data %>% group_by(ID) %>% remove_incomplete_strata(col = 'c_5mm')
#summary(data1)
#data1<- as.data.frame(data1)

# data1 <- data1 %>% #filter(case_==TRUE) %>% 
#   group_by(ID) %>%
#   mutate(season_no = n_distinct(season)) %>% filter(season_no>=2)

## season ---------------------------------------------------------------##

data1 <- data1 %>%
  nest(stps = c(-ID, -site, -season)) %>%
  mutate(stps = map(stps, ~ {
    shape <- .x$sl_dist_shape[1]
    scale <- .x$sl_dist_scale[1]
    attributes(.x)$sl_ <- make_gamma_distr(shape=shape,scale = scale)
    #kappa <- .x$ta_dist_kappa[1]
    #attributes(.x)$ta_ <- make_vonmises_distr(kappa = kappa)
    return(.x)
  }))

data1 <- data1 %>% mutate(fit = map(stps, ~ fit_issf(case_ ~ 
                #sl_ + log_sl_+ ca_5 +
                ca_5 +
                strata(step_id_), data=., model=T)))

              

calculate_RSS <- function(model) {
  # Get the fixed effects and random effects from the model
  s1 <- data.frame(#sl_ = 100,
                   #log_sl_ = log(100),
                   ca_5 = seq(from = -1, to = 1, length.out = 200))
  s2 <- data.frame(#sl_ = 100,log_sl_ = log(100),
                   ca_5 = 0)
  
  # Calculate log-RSS
  lr2 <- log_rss(model, s1, s2,ci ='se')
  
  return(lr2$df)
}

# Calculate RSS for each model

data1 <- data1 %>% mutate(log_rss = map(fit, calculate_RSS))

data1 %>% select(ID, site, season, log_rss) %>% unnest(cols = c(log_rss)) %>% #filter(season=='Dry season') %>% 
  ggplot(aes(x = ca_5_x1, y = exp(log_rss),colour=ID, group=ID, linetype = site)) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = exp(0), linetype = "dashed", color ="gray30") + 
  xlab("cloud angle") + 
  ylab("log-RSS vs cloud angle (0)") + 
  ylim(0,3)+
  theme_bw()+
  facet_wrap(~season)

data1 %>% select(ID, site, season, log_rss) %>% unnest(cols = c(log_rss)) %>% 
  group_by(ID,site,season) %>% 
  mutate(obs = row_number()) %>% 
  group_by(site, season,obs) %>% 
  summarise(ca_5_x1 = mean(ca_5_x1),
            log_rss_avg = mean(log_rss),
            log_rss_sd = sd(log_rss)) %>% 
  #filter(season=='Dry season') %>% 
  ggplot(aes(x = ca_5_x1, y = log_rss_avg,colour=site, group=site)) + 
    geom_ribbon(aes(ymin = log_rss_avg - log_rss_sd, ymax = log_rss_avg + log_rss_sd, fill = site), linetype = "dashed", alpha = 0.5) +
  geom_line(size = 1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color ="gray30") + 
  xlab("cloud angle") + 
  ylab("log-RSS vs cloud angle 90 degrees") + 
  #ylim(0,3)+
  theme_bw()+
  facet_wrap(~season)


data1 %>% mutate(coef = map(fit, ~ broom::tidy(.x$model,conf.int = T, conf.level = 0.95))) %>%
  dplyr::select(ID, site, season, coef) %>% unnest(cols = c(coef)) %>% filter(term == 'ca_5') %>% 
  ggplot(., aes(x = term, y = estimate, group = interaction(ID,season), col = site, pch=season)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "movement metrics", y = "Relative Selection Strength") +
  theme_light() #+
  #scale_y_continuous(limits = c(-2, 2))+
  #scale_x_discrete(labels = c("distance_to_tamsat_raincloud", "angle_to_tamsat_raincloud", "cos(turning_angle_difference_tamsat_raincloud)","sl_", "log_sl_","turning angle"))

###----------- months -------------------------------------##

data2 <- data %>% group_by(ID) %>% remove_incomplete_strata(col = 'c_5mm')

data2 <- data2 %>%
  nest(stps = c(-ID, -site, -months)) %>%
  mutate(stps = map(stps, ~ {
    shape <- .x$sl_dist_shape[1]
    scale <- .x$sl_dist_scale[1]
    attributes(.x)$sl_ <- make_gamma_distr(shape=shape,scale = scale)
    #kappa <- .x$ta_dist_kappa[1]
    #attributes(.x)$ta_ <- make_vonmises_distr(kappa = kappa)
    return(.x)
  }))

data2 <- data2 %>% mutate(fit = map(stps, ~ fit_issf(case_ ~ 
                                                       #sl_ + log_sl_+ ca_5 +
                                                       ca_5 +
                                                       strata(step_id_), data=., model=T)))



calculate_RSS <- function(model) {
  # Get the fixed effects and random effects from the model
  s1 <- data.frame(#sl_ = 100,
    #log_sl_ = log(100),
    ca_5 = seq(from = -1, to = 1, length.out = 200))
  s2 <- data.frame(#sl_ = 100,log_sl_ = log(100),
    ca_5 = 0)
  
  # Calculate log-RSS
  lr2 <- log_rss(model, s1, s2,ci ='se')
  
  return(lr2$df)
}

# Calculate RSS for each model

data2 <- data2 %>% mutate(log_rss = map(fit, calculate_RSS))

data2 %>% select(ID, site, months, log_rss) %>% unnest(cols = c(log_rss)) %>% #filter(season=='Dry season') %>% 
  ggplot(aes(x = ca_5_x1, y = exp(log_rss),colour=ID, group=ID, linetype = site)) + 
  geom_line(size = 1) + 
  geom_hline(yintercept = exp(0), linetype = "dashed", color ="gray30") + 
  xlab("cloud angle") + 
  ylab("log-RSS vs cloud angle (0)") + 
  ylim(0,3)+
  theme_bw()+
  facet_wrap(~months)

data2 %>% select(ID, site, months, log_rss) %>% unnest(cols = c(log_rss)) %>% 
  group_by(ID,site,months) %>% 
  mutate(obs = row_number()) %>% 
  group_by(site, months,obs) %>% 
  summarise(ca_5_x1 = mean(ca_5_x1),
            log_rss_avg = mean(log_rss),
            log_rss_sd = sd(log_rss)) %>% 
  #filter(season=='Dry season') %>% 
  ggplot(aes(x = ca_5_x1, y = log_rss_avg,colour=site, group=site)) + 
  geom_ribbon(aes(ymin = log_rss_avg - log_rss_sd, ymax = log_rss_avg + log_rss_sd, fill = site), linetype = "dashed", alpha = 0.5) +
  geom_line(size = 1) + 
  geom_hline(yintercept = 0, linetype = "dashed", color ="gray30") + 
  xlab("cloud angle") + 
  ylab("log-RSS vs cloud angle 90 degrees") + 
  #ylim(0,3)+
  theme_bw()+
  facet_wrap(~months)


data1 %>% mutate(coef = map(fit, ~ broom::tidy(.x$model,conf.int = T, conf.level = 0.95))) %>%
  dplyr::select(ID, site, months, coef) %>% unnest(cols = c(coef)) %>% filter(term == 'ca_5') %>% 
  ggplot(., aes(x = term, y = estimate, group = interaction(ID,months), col = site, pch=season)) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),position = position_dodge(width = 0.7), size = 0.8) +
  geom_hline(yintercept = 0, lty = 2) +
  labs(x = "movement metrics", y = "Relative Selection Strength") +
  theme_light() #+
#scale_y_continuous(limits = c(-2, 2))+
#scale_x_discrete(labels = c("distance_to_tamsat_raincloud", "angle_to_tamsat_raincloud", "cos(turning_angle_difference_tamsat_raincloud)","sl_", "log_sl_","turning angle"))
