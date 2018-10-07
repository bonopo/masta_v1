
# Masterarbeit ------------------------------------------------------------


# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
# install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie"))
# install.packages("drought", repos="http://R-Forge.R-project.org")
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "drought", "lubridate", "dplyr"), require, character.only = T)

# Load data ---------------------------------------------------------------

load("./data/catchments/eobs_pr_part.Rdata")
load("./data/catchments/eobs_temp_part.Rdata")
load("./data/catchments/streamflow.Rdata")
#gauges  <- readOGR(dsn="./data/raster/gauges", layer= "gauges")
gauges  <- shapefile("./data/raster/gauges")

raster(gauges)
# overview ----------------------------------------------------------------
#gauges
plot(gauges) 
str(gauges)

#writing loading function

load_file <- function(file, value_name){
  output <- melt(file, varnames = c("date", "gauge"), value.name = value_name )
  seq_date <- seq.Date(from= as.Date("1970-1-1"),by=1, length.out = diff(range(output$date))+1) %>% 
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
temp <- load_file(file=tempera, value_name = "temp")
  
# temp %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=temp, colour=as.factor(gauge), group=gauge), se=F)



# Cluster calculation -----------------------------------------------------
#seasonality ratio
q_sr_w <- q_long %>% 
  mutate(month = month(date)) %>% 
  filter(month > 11 | month <4) %>% 
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
q_sr$sr <- q_sr$q95_s/q_sr$q95_w

q_sr$sr_value[which(q_sr$sr < 1)] <- 0 #summer
q_sr$sr_value[which(q_sr$sr > 1)] <- 1 #winter

gauges$sr <- as.numeric(q_sr$sr_value)
spplot(gauges, "sr")
  
 
# SPI calculation ---------------------------------------------------------


p<- q_long %>% 
  filter(gauge==150)

