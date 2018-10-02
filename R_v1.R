
# Masterarbeit ------------------------------------------------------------


# Preambel ----------------------------------------------------------------
getwd()
setwd()
install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2"))
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2"), require, character.only = T)

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

#  filter(gauge < 10) %>%
ggplot()+
  geom_smooth(aes(x=date, y=daily_sum, colour=as.factor(gauge), group=gauge), se=F)


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
