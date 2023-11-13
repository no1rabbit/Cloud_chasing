library(tidyverse)
library(lubridate)
library(beepr)
library(readr)
library(sf)

#Function
UTMtoLongLat<-function(x,y,inproj,outproj){
  xy <- data.frame(ID = 1:length(x), X = x, Y = y)
  df_sf <- st_as_sf(x = xy,                         
                    coords = c("X", "Y"),
                    crs =     inproj)
  res <- df_sf %>% st_transform(outproj)
  res <- res %>%
    mutate(X = unlist(map(res$geometry,1)),
           Y = unlist(map(res$geometry,2)))
  return(res)
}



#Serengeti_data <- read_csv("data/WB_ZB_EL_SNP_DATA.csv", col_types = cols(Date = col_datetime(format = "%d/%m/%Y %H:%M")))

Serengeti_data <- read_csv("WB_ZB_EL_SNP_DATA_1999-02-25_2022-06-08_Deads_deleted.csv", 
                                                 col_types = cols(Date = col_datetime(format = "%d/%m/%Y %H:%M")))


Serengeti_data <- Serengeti_data %>% 
  mutate(AID = recode(AID,'T5H-5571' = 'W111',
                         'T5H-6142' = 'Z39',
                         'T5H-5600' = 'W112')) %>% 
  #filter(SPECIES == 'WB') %>% 
  mutate(timestamp = as.POSIXct(Date),
         migrant = if_else(migrant == 0, 'r','m'),
         lat = NA,
         long = NA,
         temperature = NA,
         CRS = 21036,
         ID = paste0('S',AID),
         sex = SEX,
         owner = OWNER,
         species = SPECIES,
         site = 'Serengeti') %>% 
  filter(owner != 'STABACH') %>%
  filter(!is.na(x) &!is.na(y)) %>% 
  filter(ID !='SZ39') %>% 
  select(ID, species, x, y, lat, long, timestamp, sex, migrant)

new<-UTMtoLongLat(Serengeti_data$x,Serengeti_data$y,21036,4326)
Serengeti_data$lat<-new$Y; Serengeti_data$long<-new$X
remove(new)
write_csv(Serengeti_data, 'data/Serengeti_all.csv', na='')
#write_csv(Serengeti_data, 'cleaned_data/Serengeti_data.csv', na='')

#### complete data
data <-
  list.files(path = "C:/Users/boyer/OneDrive - University of Glasgow/Documents/ISSF_analysis/data/Data_cleanup/data/cleaned_data/", pattern = "*.csv", full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(ID = col_character(),sex = col_character())))
data1 <- data %>% distinct(ID, x, y,timestamp, .keep_all = TRUE)

write_csv(data1, 'cleaned_data/wildebeest_data.csv', na='')

data1 %>% group_by(ID,site,owner,sex) %>% 
  tally() %>% print(n=500)


library(plotly)
b<-ggplot(data1, aes(x=long, y=lat,colour = ID))+
  geom_point()

ggplotly(b)
