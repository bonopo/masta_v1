
# Drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
source("./R/masta_v1/data_handling.R")
source("./R/masta_v1/functions.R")


#month with max drought overall####


#with actuall flow data
year = year(date_seq) %>% list()
mt_mn_q$year <- unlist(rep(year, times = catch_n))
mnq30 <- aggregate(mt_mn_q_wide, by= year, FUN= min, by.column=T)

mnq30_month <- c()
for ( i in 1:338){
data <- mt_mn_q %>% 
  filter(gauge == i)
data_by <- data %>% group_by(year(yr_mt)) %>% 
  summarise(mon_min = month(yr_mt[which.min(q_mean)]))  
  mnq30_month[i] <- names(which.max(table(data_by$mon_min)))
}

plot(mnq30_month)


#comparing ssi data with real q data. are they similar?
 ssi_wide %>% 
    group_by(month, gauge) %>% 
    summarise(q_mean = mean(ssi)) %>% 
  filter(gauge==200) %>% 
  ggplot()+
  geom_point(aes(y=q_mean, x=month), na.rm = T)+
   geom_line(data =    mt_mn_q %>% 
    group_by(month, gauge) %>% 
    summarise(q_mean = mean(q_mean)) %>% 
  filter(gauge==200), aes(y=q_mean, x=month))
    
   


#mnq30 30 day window####
mq30 <-  rollapply(q_wide, width = 30, FUN = mean, by.column=TRUE ,fill = NA, align="center") %>% as.data.frame()

mq30$date <- q_long$date[q_long$gauge == 1]
#mnq30
mnq30 <- aggregate.data.frame(mq30, by = list(years), min, na.rm = TRUE)

mnq30_long <- gather(mq30, key=gauge, value= mq30, -date ) %>% as.tbl()

#mnq30 date

mnq30_date <- mnq30_long %>% 
  group_by(year(date), gauge) %>% 
  summarise(date[which.min(mq30)]) %>% 
  ungroup()

colnames(mnq30_date) <- c("year", "gauge", "date_mnq30")

mnq30_date %>% 
  filter(gauge == 100) 




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
 

#lengths of severe droughts ####
#defining drought: ssi < -2

dr_length_1 <- dr_length()
dr_length_2 <- dr_length(severity = -2)
dr_length_3 <- dr_length(severity = -3)
 
plot(dr_length_2[[80]])

#drought events per decade #### 
dr_n_1 = dr_n()
plot(dr_n_1[[50]])

# drought intensity####
# i.Drought intensity (Id): it is the average value of a drought parameter below the critical level. It is measured as the drought severity divided by the duration.




#drought severity ####
#sum of differences between ssi indicator and threshold
#Drought severity (Sd): it indicates a cumulative deficiency of a drought parameter below the critical level. 



dsi_1 <- dr_severity()

