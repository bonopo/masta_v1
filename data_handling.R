# Startin script
# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
#save.image(file="./data/r_temp_image/line447.Rdata")
# load(file="./data/r_temp_image/line307.Rdata")

#install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "SPEI", "eha","reliaR", "PearsonDS","FAdist","trend", "Kendall","mgcv", "CTT"))
# install.packages("drought", repos="http://R-Forge.R-project.org")
#install.packages("tidyverse")
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "lubridate", "SPEI", "lmomco",  "evd", "reliaR", "PearsonDS", "FAdist","trend","Kendall", "mgcv", "lmtest","lfstat", "CTT"), require, character.only = T)
#library(tidyverse) 

# User defined constants --------------------------------------------------

agg_month <- 12 #spei-n and spi-n how many month should be max aggregation
date_seq <- seq.Date(from= ymd("1970-01-15"), to = ymd("2009-12-15"), by="month")  
catch_n <- 338


# Load data ---------------------------------------------------------------

source("./R/masta_v1/functions.R")
load("./data/catchments/eobs_pr_part.Rdata")
load("./data/catchments/eobs_temp_part.Rdata")
load("./data/catchments/streamflow.Rdata")
#gauges  <- readOGR(dsn="./data/raster/gauges", layer= "gauges")
gauges  <- shapefile("./data/raster/gauges")

# raster(gauges)
# overview ----------------------------------------------------------------

#precip
colnames(precip) <- 1:catch_n
precip_long <- load_file(precip, "sum_mm")
unique(precip_long$gauge)
mt_sm_p <- precip_long %>%
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>%
  group_by(gauge,yr_mt) %>%
  summarise(month_sum = sum(sum_mm)) %>%
  ungroup()


mt_sm_p_wide <- spread(mt_sm_p, key=gauge, value=month_sum, drop=F) %>% dplyr::select(-yr_mt) %>% as.data.frame()


# precip_long %>% 
#   filter(gauge < 10) %>%
#   ggplot()+
#     geom_smooth(aes(x=date, y=sum_mm, colour=as.factor(gauge), group=gauge), se=F)

#discharge
q_long <- load_file(streamflow, "q")
q_wide <- spread(q_long, key= gauge, value = q)
q_wide %<>% dplyr::select(-date) %>% as.data.frame()
colnames(q_wide) <- c(1:catch_n)
mt_mn_q <- q_long %>% 
    mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>% 
  group_by(gauge,yr_mt) %>% 
  summarise(q_mean = mean(q)) %>% 
  ungroup() %>% 
  mutate(month = month(yr_mt))

mt_mn_q_wide <- spread(mt_mn_q, key = gauge, value = q_mean) %>% dplyr::select(-c(yr_mt,month)) %>% as.data.frame()
 #the gauge numbers are not equal to the gauge numbers in temperature and discharge 


# q_long %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=q, colour=as.factor(gauge), group=gauge), se=F)

#temperature
colnames(tempera) <- 1:catch_n
temp_long <- load_file(file=tempera, value_name = "temp", origin = "1950-01-01")
temp_long %<>% filter(date>= "1970-01-01" & date <= "2009-12-31") 
mt_mn_temp <- temp_long %>%
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>%
  group_by(gauge, yr_mt) %>%
  summarise(temp_m = mean(temp)) %>%
  ungroup()

# temp %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=temp, colour=as.factor(gauge), group=gauge), se=F)


