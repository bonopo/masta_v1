
# Drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")




# seasonal data ####
summer_ave_q = seas_cl(data_source = "mt_mn_q", method = "mean", value = "q_mean", begin =4, end=10) 
summer_min_q = seas_cl(data_source = "mt_mn_q", method = "min", value = "q_mean", begin =4, end=10) #summer mnq30

summer_sum_p = seas_cl(data_source = "mt_sm_p", method = "sum", value = "month_sum")
winter_sum_p = seas_cl(data_source = "mt_sm_p", method = "sum", value = "month_sum", begin = 11, end =3)

summer_q_q10 =  mt_mn_q %>% 
  filter(month(yr_mt) >= 4, month(yr_mt)<= 10) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(q10 = quantile(q_mean, .1)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q10) %>% 
  dplyr::select(-`year(yr_mt)` ) %>% 
  as.data.frame()

summer_q = mt_mn_q %>% 
  filter(month(yr_mt) >= 4, month(yr_mt)<= 10) %>% 
  spread(key=gauge, value=q_mean) %>% 
  dplyr::select(-yr_mt, -month, -year) %>% 
  as.data.frame()

winter_q = mt_mn_q %>% 
  filter(month(yr_mt) < 4 | month(yr_mt) > 10) %>%
  spread(key=gauge, value=q_mean) %>% 
   dplyr::select(-yr_mt, -month, -year) %>% 
   as.data.frame()


#yearly data ####
yearly_mean_q = mt_mn_q %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_mean = mean(q_mean)) %>% 
  spread(key=gauge, value=yearly_mean) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

yearly_min_q = mt_mn_q %>%  #same as mnq30
  group_by(gauge, year) %>% 
  summarise(yearly_min = min(q_mean)) %>% 
  spread(key=gauge, value=yearly_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

yearly_q10 = mt_mn_q %>%  #same as mnq30
  group_by(gauge, year) %>% 
  summarise(yearly_min = quantile(q_mean,.1)) %>% 
  spread(key=gauge, value=yearly_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()


#month with max drought overall####


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



gauges$mnq30_month = mnq30_month
#monthly data ####
nq_monthly = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_min = min(q)) %>% 
  spread(key = gauge, value=monthly_min) %>% 
  dplyr::select(-yr_mt) %>% 
  as.data.frame()


#number of months in a year affected by drought ####


dr_length_1 <- dr_n()
dr_length_1_5 <- dr_n(severity = -1.5) #min value is -1.97

 


#drought severity & intensity####
#severity: sum of differences between ssi indicator and threshold
#Drought severity (Sd): it indicates a cumulative deficiency of a drought parameter below the critical level. 
# i.Drought intensity (Id): it is the average value of a drought parameter below the critical level. It is measured as the drought severity divided by the duration.
# or maybe max deviation from treshhold per year ()

dr_event_no_1<- dr_count(severity = -1)

dsi_1<- dr_severity(severity = -1)

dsi_1_yearly = list()
for (i in 1:catch_n){
dsi_1_yearly[[i]] = dsi_1[[i]] %>% 
  mutate(year = year(dr_start)) %>% 
  group_by(year) %>% 
  summarise(mean_dsi = mean(dsi), mean_length = mean(dr_length), mean_inten = mean(dr_intens))

}


  


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
 



 
 
 
 
