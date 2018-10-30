
# Drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")

#month with max drought overall####


#with actuall flow data
year = year(date_seq) %>% list()
mt_mn_q$year <- unlist(rep(year, times = catch_n))
mnq30 <- aggregate(mt_mn_q_wide, by= year, FUN= min, by.column=T)

mnq30_long <- gather(mq30, key=gauge, value= mq30, -date ) %>% as.tbl()

#mnq30 date

mnq30_date <- mnq30_long %>% 
  group_by(year(date), gauge) %>% 
  summarise(date[which.min(mq30)]) %>% 
  ungroup()

colnames(mnq30_date) <- c("year", "gauge", "date_mnq30")



mnq30_month <- c()
for ( i in 1:338){
data <- mt_mn_q %>% 
  filter(gauge == i)
data_by <- data %>% group_by(year(yr_mt)) %>% 
  summarise(mon_min = month(yr_mt[which.min(q_mean)]))  
  mnq30_month[i] <- names(which.max(table(data_by$mon_min))) %>% as.integer()
}

plot(mnq30_month)

gauges$mnq30_month = mnq30_month

# #comparing ssi data with real q data. are they similar?
#  ssi_1 %>% 
#     group_by(month, gauge) %>% 
#     summarise(q_mean = mean(ssi)) %>% 
#   filter(gauge==200) %>% 
#   ggplot()+
#   geom_point(aes(y=q_mean, x=month), na.rm = T)+
#    geom_line(data =    mt_mn_q %>% 
#     group_by(month, gauge) %>% 
#     summarise(q_mean = mean(q_mean)) %>% 
#   filter(gauge==200), aes(y=q_mean, x=month))
    
   


#month in a year affected by drought ####


dr_length_1 <- dr_n()
dr_length_1_5 <- dr_n(severity = -1.5) #min value is -1.97

 


#drought severity & intensity####
#severity: sum of differences between ssi indicator and threshold
#Drought severity (Sd): it indicates a cumulative deficiency of a drought parameter below the critical level. 
# i.Drought intensity (Id): it is the average value of a drought parameter below the critical level. It is measured as the drought severity divided by the duration.
# or maybe max deviation from treshhold per year ()

dr_event_no_1<- dr_count(severity = -1)

dsi_1<- dr_severity(severity = -1)


#measure of distance to june to overcome problem 12 - 1####
 
plot(x= dr_beg[[15]]$mon_min, ylim= c(4,12), type="p")
 
 6-dr_beg[[i]]$mon_min[1:10] %>% mean() # for decade
 # you get weird averages
 
#with month with most severe ssi ?
 
 
 ssi_wide %>% 
   mutate(year = year(date)) %>% 
   filter(gauge == i, ssi < -1) %>% 
   group_by(year) %>% 
   summarise(which.min(ssi))
 


