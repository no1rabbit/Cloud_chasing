library(momentuHMM)
library(tidyverse)
library(lubridate)
library(amt)
library(readr)

### HMM model for daily summed movement


data <- read_csv('data/Serengeti_all.csv')
data<-data %>%
  group_by(ID) %>%
  arrange(date, .by_group = TRUE) 
data<-as.data.frame(data)

data<-data %>% filter(species == 'WB' & migrant =='migrant') %>% 
  filter(case_ == TRUE)

data<-prepData(data,type = "LL",coordNames = c("lat","long"), covNames = c('sex','x1_','y1'))
summary(data)

ggplot(data, aes(date,step))+
  geom_line()+
  geom_hline(yintercept = 3,colour = 'blue',linewidth = 1) +
  geom_hline(yintercept = 10, colour='green',linewidth = 1)

set.seed(1234)
ncores <- 8 # number of cores for parallel processing
retryFits <- 20 # number of attempts to improve likelihood using random pertubation
nbStates <- 2
stepDist <- "gamma" 
angleDist <- "vm" 
stateNames <- c("encamped","migrating")
mu0 <- c(3,10)
sigma0 <- c(3,10)
#zeromass0 <- c(0.01,0.001,0.001)
kappa0 <- c(0.1,10)
stepPar <- c(mu0,sigma0)
anglePar <- kappa0 
formula <- ~ID

d_gammaHMM <- fitHMM(data = data, nbStates=nbStates, dist=list(step=stepDist,angle=angleDist), Par0=list(step=stepPar,angle=anglePar), estAngleMean = list(angle=FALSE), stateNames = stateNames, formula=formula, retryFits=retryFits, ncores=ncores)

d_gammaHMM
plotPR(d_gammaHMM)
plot(d_gammaHMM, plotCI=T,ask=F, plotTracks=T,breaks = 100)

data <- data %>% 
  mutate(dailystate = viterbi(d_gammaHMM),
         dailystate = case_when(dailystate == 1 ~ "encamped", 
                                dailystate == 2 ~'migrating'),
         dailystate=factor(dailystate,levels=c("encamped","migrating")))

data <- data %>% 
  mutate(year = year(date),
         months = month(date,label=T,abbr=T),
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
         ))

data %>% 
  filter(ID == 'SW65') %>% 
  ggplot(aes(x, y)) +
  geom_path(aes(color = season, linetype = dailystate)) +
  geom_point() +
  facet_wrap(~ID)

write_csv(data, file='serengeti_wb_HMM.csv',na='')
