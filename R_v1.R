
# Masterarbeit ------------------------------------------------------------


# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
#install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "SPEI", "eha"))
# install.packages("drought", repos="http://R-Forge.R-project.org")
# devtools::install_github("hadley/dplyr")
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "drought", "lubridate", "SPEI", "lmomco", "dplyr", "evd"), require, character.only = T)
library(eha)

# Load data ---------------------------------------------------------------

load("./data/catchments/eobs_pr_part.Rdata")
load("./data/catchments/eobs_temp_part.Rdata")
load("./data/catchments/streamflow.Rdata")
#gauges  <- readOGR(dsn="./data/raster/gauges", layer= "gauges")
gauges  <- shapefile("./data/raster/gauges")

raster(gauges)
# overview ----------------------------------------------------------------
#gauges
# plot(gauges) 
# str(gauges)

#writing loading function

load_file <- function(file, value_name, origin="1970-1-1"){
  output <- melt(file, varnames = c("date", "gauge"), value.name = value_name )
  seq_date <- seq.Date(from= as.Date(origin),by=1, length.out = diff(range(output$date))+1) %>% 
  rep(., times=length(unique(output$gauge)))
  output %<>%
  mutate(date = seq_date) %>% 
  mutate(gauge = parse_number(gauge)) %>% 
  mutate(gauge =as.integer(gauge)) %>% 
  as.tibble()
  return(output)
}

#precip
precip_long <- load_file(precip, "sum_mm")

# precip_long %>% 
#   filter(gauge < 10) %>%
#   ggplot()+
#     geom_smooth(aes(x=date, y=sum_mm, colour=as.factor(gauge), group=gauge), se=F)


#discharge
q_long <- load_file(streamflow, "q")

# q_long %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=q, colour=as.factor(gauge), group=gauge), se=F)

#temperature
temp_long <- load_file(file=tempera, value_name = "temp", origin = "1950-01-01")
temp_long %<>% filter(date>= "1970-01-01" & date <= "2009-12-31") 


# temp %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=temp, colour=as.factor(gauge), group=gauge), se=F)



# Cluster calculation -----------------------------------------------------
#seasonality ratio (SR)

# creating two time series one winter one summer
#calculating q95 for both parts
q_sr_w <- q_long %>% 
  mutate(month = month(date)) %>% 
  filter(month > 11 | month <4) %>%  #definitin of winter from Laaha et al 2006
  group_by(gauge) %>% 
  mutate(qt = quantile(q, 0.05)) %>% 
  summarise(q95_w = mean(qt))

q_sr_s <- q_long %>% 
  mutate(month = month(date)) %>% 
  filter(month < 12 | month > 3) %>% 
  group_by(gauge) %>% 
  mutate(qt = quantile(q, 0.05)) %>% 
  summarise(q95_s = mean(qt))

q_sr <- merge(q_sr_s, q_sr_w, by="gauge")
q_sr$sr <- q_sr$q95_s/q_sr$q95_w # SR is caclulated via q95_summer/q95_winter from Laaha et al 2006

q_sr$sr_value[which(q_sr$sr < 1)] <- 0 #summer
q_sr$sr_value[which(q_sr$sr > 1)] <- 1 #winter NAs are produced in a 9 time series that are very altered or have no clear seasonality 

gauges$sr <- as.numeric(q_sr$sr_value)
# spplot(gauges, "sr")
  
# ind <- which(is.na(q_sr$sr_value))
# q_long %>%
#   filter(gauge %in% ind) %>%
#   filter(year(date) < 1975) %>%
#   ggplot() +
#   geom_smooth(aes(x=date, y=q, group= as.factor(gauge), color= as.factor(gauge)))+
#   scale_y_log10()


# SPI calculation ---------------------------------------------------------


# # rough estimate of gamma distribution parameters
# para_ini <- precip_long %>% 
#   group_by(gauge) %>% 
#   summarise(shape_p = dist.start(sum_mm, "gamma")[[1]], rate_p = dist.start(sum_mm, "gamma")[[2]] )

#aggregating into montly sums

precip_monthly <- precip_long %>% 
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>% 
  group_by(gauge,yr_mt) %>% 
  summarise(month_sum = sum(sum_mm)) %>% 
  ungroup()

#1. Step: fitting of distribution to aggregated data with SCI package 
#2. Step: transform to standart normal distribuion with mean: 0 and sd: 1
#with SCI package
spi <- as.list(NA)
for(i in unique(precip_monthly$gauge)){
data <- precip_monthly$month_sum[precip_monthly$gauge==i]
params_dist <- fitSCI(data, first.mon = 1, distr = "gamma", time.scale = 6, p0 =T)
#inital values for parameters calculated with L-moments and then optimization with maximum likelihood
spi_temp <- transformSCI(data, first.mon = 1, obj = params_dist)
spi[[i]] <- spi_temp
}


# plot(spi[[1]], type="l")


# SPEI Calculation --------------------------------------------------------
#with SPEI package and SCI

# PET calculation with thornwaite
mt_mn_temp <- temp_long %>% 
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(temp_m = mean(temp)) %>% 
  ungroup()


#Gauss Krueger converted to WGS84
xy_gk <- cbind.data.frame("x_gk" = 4475806, "y_gk"= gauges$Hochwrt) #only latitude is needed, therefore random x value
coordinates(xy_gk) <-  c("x_gk", "y_gk")
proj4string(xy_gk) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
xy_wgs84 <- spTransform(xy_gk, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


latitude <- coordinates(xy_wgs84) %>% 
  as.data.frame() %>% 
  dplyr::select(y_gk) %>% 
  cbind("gauge" = unique(mt_mn_temp$gauge)) %>% 
  as.tbl()

#calculating Potential Evapotranspiration with thornthwaite
pet_th <- list(NA)
for(i in unique(mt_mn_temp$gauge)){
  data <- mt_mn_temp$temp_m[mt_mn_temp$gauge==i]
  res_ts <- ts(data, frequency=12, start=c(1970,1))
  pet_th[[i]] <- thornthwaite(data,latitude$y_gk[latitude$gauge==i] )
}

#calculating SPEI: P - PET
pet_th_vec <- pet_th[unique(mt_mn_temp$gauge)] %>% 
  unlist() %>% 
  as.numeric()
spei_data <-precip_monthly %>% 
    mutate(pet_th = pet_th_vec) %>% 
    mutate(p_pet = month_sum - pet_th)

# with Generalized Logistic Distribution                  
spei_gl <- as.list(NA)
for(i in unique(spei_data$gauge)){
data <- spei_data$p_pet[spei_data$gauge==i]
params_dist <- fitSCI(data, first.mon = 1, distr = "genlog", time.scale = 6, p0 =F)
#inital values for parameters calculated with L-moments and then optimization with maximum likelihood
spi_temp <- transformSCI(data, first.mon = 1, obj = params_dist)
spei_gl[[i]] <- spi_temp
}    

#with Generalized extreme value distribution
spei_gev <- as.list(NA)
for(i in unique(spei_data$gauge)){
data <- spei_data$p_pet[spei_data$gauge==i]
params_dist <- fitSCI(data, first.mon = 1, distr = "gev", time.scale = 6, p0 =F)
#inital values for parameters calculated with L-moments and then optimization with maximum likelihood
spi_temp <- transformSCI(data, first.mon = 1, obj = params_dist)
spei_gev[[i]] <- spi_temp
}   

#with log-logistic distribution and different function (SPEI package)

spei_llogis <- as.list(NA)
for(i in unique(spei_data$gauge)){
data <- spei_data$p_pet[spei_data$gauge==i]
spei_llogis[[i]] <- spei(data, scale=6)
}   

#converting SPEI objects into vectors
spei_vec <- function(data, spei=FALSE){
  output <-  vector()
  for(i in unique(mt_mn_temp$gauge)){
  temp <- unclass(data[[i]]) 
  if(spei == FALSE){
    output <- c(output, as.numeric(temp))}else{
    output <- c(output, as.numeric(temp$fitted))}
  }
  return(output)
  }

spei_ll_v <- spei_vec(spei_llogis,spei= TRUE)
spei_gv_v <- spei_vec(spei_gev)
spei_gl_v <- spei_vec(spei_gl)


#merging data

spei <- cbind(spei_ll_v, spei_gv_v, spei_gl_v) %>% 
  as.data.frame() %>% 
  as.tbl() %>% 
  mutate(date= mt_mn_temp$yr_mt)
  




