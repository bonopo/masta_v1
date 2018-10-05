
# Masterarbeit ------------------------------------------------------------


# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie"))
install.packages("drought", repos="http://R-Forge.R-project.org")
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "drought", "lubridate", "dplyr"), require, character.only = T)

# Load data ---------------------------------------------------------------

load("./data/catchments/eobs_pr_part.Rdata")
load("./data/catchments/eobs_temp_part.Rdata")
load("./data/catchments/streamflow.Rdata")
gauges  <- readOGR(dsn="./data/raster/gauges", layer= "gauges")



# overview ----------------------------------------------------------------
#gauges
plot(gauges) 

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

precip_long %>% 
  filter(gauge < 10) %>%
  ggplot()+
    geom_smooth(aes(x=date, y=sum_mm, colour=as.factor(gauge), group=gauge), se=F)


#discharge
q_long <- load_file(streamflow, "q")

q_long %>% 
  filter(gauge < 10) %>%
ggplot()+
  geom_smooth(aes(x=date, y=q, colour=as.factor(gauge), group=gauge), se=F)

#temperature
temp <- load_file(file=tempera, value_name = "temp")
  
temp %>% 
  filter(gauge < 10) %>%
ggplot()+
  geom_smooth(aes(x=date, y=temp, colour=as.factor(gauge), group=gauge), se=F)



# Cluster calculation -----------------------------------------------------
q_long %<>% 
  mutate(month = month(date)) %>% 
  mutate(winter= month > 11 | month <4) %>% 
  mutate(lf_s = 0) %>% 
   mutate(lf_w = 0) %>%
  group_by(gauge) %>% 
  mutate(qt = quantile(q, 0.05)) 

lf_w_ind <- which(q_long$qt >= q_long$q & q_long$winter==TRUE)
q_long$lf_w[lf_w_ind] <-  q_long$q[lf_w_ind]

lf_s_ind <- which(q_long$qt >= q_long$q & q_long$winter==FALSE)
q_long$lf_s[lf_s_ind] <-  q_long$q[lf_s_ind]
  




# SPI calculation ---------------------------------------------------------


mtcars %>% 
  select(cyl)

