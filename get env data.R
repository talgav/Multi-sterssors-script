rm(list = ls())
.rs.restartR()
gc()

# Load package 
library(sdmpredictors)
library(tidyverse)


load("Data\\Clean_Medata_lis.Rdata")

# medata <- read_rds("data/medata.Rds")

# explore layers from Bio ORACLE

all_bio_layers <- sdmpredictors::list_layers(datasets = "Bio-ORACLE", marine = TRUE) %>%
  dplyr::select(layer_code, name) # to view layers names and code

# to search for a layer code by a specific string - change the 2nd arg in str_detect:
all_bio_layers %>% filter(stringr::str_detect(.$name, "temperature"))
all_bio_layers %>% filter(stringr::str_detect(.$name, "salinity"))
all_bio_layers %>% filter(stringr::str_detect(.$name, "Primary production"))

# Download environmental layers -------------------------------------------

my.sites <- medata %>% dplyr::select(Site, lon, lat)

environment.layers <- sdmpredictors::load_layers(layercodes = c("BO_sstmean", # temperature
                                                                "BO2_salinitymean_ss",  # salinity
                                                                "BO22_ppmean_ss"), # productivity
                                                 datadir = "data/bio_oracle")
# # If there's an error with file downloading due to timeout limit, run the following code then attempt to download layers again:
# options(timeout = max(300, getOption("timeout")))

# Extract environmental values for sites in medata ------------------------

my.sites.environment <- bind_cols(my.sites, 
                                  raster::extract(environment.layers, my.sites[,2:3]))
my.sites.environment

# Check for NAs
my.sites.environment %>% 
  dplyr::filter(!complete.cases(.)) %>% 
  dplyr::distinct() %>% summarise(sites_with_NA = n())


no_temp<-my.sites.environment %>% filter(is.na(BO_sstmean)) %>% distinct()

sites_test<-my.sites.environment %>%
  filter(Site %in% no_temp$Site) %>% distinct() %>% arrange(Site)

sites_test<-sites_test %>% 
  group_by(Site) %>%
  mutate("mean_temp"=mean(BO_sstmean,na.rm = T),
         "mean_sal"=mean(BO2_salinitymean_ss,na.rm = T),
         "mean_pp"=mean(BO22_ppmean_ss,na.rm = T))

sites_test<-sites_test %>% filter(is.na(BO_sstmean)) %>%
  select(-BO_sstmean,-BO2_salinitymean_ss,-BO22_ppmean_ss)

colnames(sites_test)<-colnames(no_temp)


env_var<-my.sites.environment %>% distinct() %>% drop_na(BO_sstmean)

env_var<-bind_rows(env_var,sites_test)

env_var<-env_var %>% arrange(Site)

colnames(env_var)<-c("Site","lon","lat","tmean","sal_mean","pp_mean")

nrow(medata %>% select(Site,lon,lat) %>% distinct())

#save(env_var,file = "Data\\env_var.Rdata")
