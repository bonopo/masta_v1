
# Drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!




# seasonal data ####
#7 day moving average in summer than calculate :
  # min for every year
  # date of min
q_wide_summer = q_long %>% 
    filter(month(date) >= 5 & month(date)<= 11) %>% 
    group_by(gauge, year(date)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q) %>% 
  as.data.frame() %>% 
  dplyr::select(-`date`, -`year(date)`)

ms7 = rollapply(q_wide_summer, FUN = mean, width = 7, align="right", by.column=TRUE, fill=NA)

int= rep(c(1:40*214),each =7)-c(207:213)#where the rollapply result hast to be NA because it calculates moving average continues (ie end of 1970 will be partially used to calculate moving average for beginning 1971)

ms7[int,] = NA 
ms7_df = as.data.frame(ms7)

ms7_min_temp = q_long %>% 
    filter(month(date) >= 5 & month(date)<= 11) %>% 
    group_by(gauge, year(date)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q) %>% 
  as.data.frame() %>% 
  dplyr::select(`date`, `year(date)`) 

ms7_min = cbind(ms7_min_temp, ms7) %>% 
  gather(key=gauge, value=ms7, -`date`, -`year(date)`) %>% 
  group_by( as.integer(gauge),year(date)) %>% 
  summarise(ms7_min = min(ms7, na.rm=T), ms7_date = yday(date[which.min(ms7)] )) %>% 
  ungroup() %>% 
  as.data.frame()

colnames(ms7_min) = c("gauge", "year", "ms7_min", "ms7_date")
remove(ms7_min_temp)

ms7_date = ms7_min %>% 
  dplyr::select(gauge, ms7_date, year) %>% 
  spread(key=gauge, value=ms7_date) %>% 
  dplyr::select(-year)

ms7_min %<>%  dplyr::select(gauge, ms7_min, year) %>% 
  spread(key=gauge, value=ms7_min) %>% 
  dplyr::select(-year) 


#30 day moving average in summer than calculate :
  # min for every year
  # date of min


ms30 = rollapply(q_wide, FUN = mean, width = 30, align="right", by.column=TRUE, fill=NA)

ms30_df = ms30 %>% 
  as.data.frame() 

ms30_df$date = ymd(q_long$date[q_long$gauge == 1])

ms30_summer = ms30_df %>% 
  filter(ymd(paste0("1960-",month(date),"-",day(date))) >= ymd("1960-5-30") & month(date)<= 11) %>% 
  mutate(year= year(date)) %>% 
  as.tbl() %>% 
  gather(key=gauge, value=ms30, -year, -date) %>% 
  group_by(gauge, year) %>% 
  summarise(ms30_min = min(ms30, na.rm=T), ms30_date = yday(date[which.min(ms30)]))%>%   ungroup() %>% 
  mutate(gauge= as.integer(gauge))

  


ms30_date = ms30_summer %>% 
  dplyr::select(gauge, ms30_date, year) %>% 
  spread(key=gauge, value=ms30_date) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

ms30_min = ms30_summer %>% 
  dplyr::select(gauge, ms30_min, year) %>% 
  spread(key=gauge, value=ms30_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

#summer mean q

summer_ave_q = summer_cl(data_source = "mt_mn_q", method = "mean", value = "q_mean", begin =5, end=11) 
summer_min_q = summer_cl(data_source = "mt_mn_q", method = "min", value = "q_mean", begin =5, end=11) #summer mnq30

summer_sum_p = summer_cl(data_source = "mt_sm_p", method = "sum", value = "month_sum", begin =5, end=11)

summer_q10 =  q_long %>% 
  filter(month(date) >= 5, month(date)<= 11) %>% 
  mutate(year=year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(q10 = quantile(q, .1)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q10) %>% 
  dplyr::select(-year ) %>% 
  as.data.frame()

summer_q = mt_mn_q %>% 
  filter(month(yr_mt) >= 4, month(yr_mt)<= 10) %>% 
  spread(key=gauge, value=q_mean) %>% 
  dplyr::select(-yr_mt, -month) %>% 
  as.data.frame()

winter_q = mt_mn_q %>% 
  filter(month(yr_mt) < 4 | month(yr_mt) > 10) %>%
  spread(key=gauge, value=q_mean) %>% 
   dplyr::select(-yr_mt, -month) %>% 
   as.data.frame()

winter_q10 = q_long %>% 
  filter(month(date) < 5 | month(date) > 11) %>%
  mutate(year=year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(q10 = quantile(q,.1)) %>% 
  spread(key=gauge, value=q10) %>% 
   dplyr::select( -year) %>% 
   as.data.frame()


#yearly data ####
yearly_mean_q = q_long %>% 
  mutate(year= year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_mean = mean(q)) %>% 
  spread(key=gauge, value=yearly_mean) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

yearly_min_q = q_long %>%  #same as mnq30
  mutate(year= year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_min = min(q)) %>% 
  spread(key=gauge, value=yearly_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

yearly_q10 = q_long %>% 
  mutate(year= year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_min = round(quantile(q,.1),4)) %>% 
  spread(key=gauge, value=yearly_min) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()



#month with max drought overall####



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
monthly_nq = q_long %>%  #not usable rather use mnq7
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_min = min(q)) %>% 
  spread(key = gauge, value=monthly_min) %>% 
  dplyr::select(-yr_mt) %>% 
  as.data.frame()

monthly_mean = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = mean(q)) %>% 
  spread(key = gauge, value=monthly_mean) %>% 
  dplyr::select(-yr_mt) %>% 
  as.data.frame()

monthly_mean_t = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = mean(q)) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_mean_t, c("gauge", "month"))
 int = seq(1,4045,12)
 for(i in 1:12){
 df=  Reduce(rbind,res[int]) %>% spread(key=gauge,value= monthly_mean) %>% dplyr::select(-yr_mt, -month)
 assign(paste0(str_to_lower(month.abb[i]),"_mean_df"),df)
 int = int+1
 }
remove(res, int, monthly_mean_t)

#number of months in a year affected by drought ####


dr_length_1 <- dr_n()
dr_length_1_5 <- dr_n(severity = -1.5) #min value is -1.97

#drought severity & intensity####


#severity: sum of differences between ssi indicator and threshold, it indicates a cumulative deficiency of a drought parameter below the critical level. 
#intensity: it is the average value of a drought parameter below the critical level. It is measured as the drought severity divided by the duration.


dr_event_no_1<- dr_count(severity = -1)

dsi_1.5<- dr_severity(severity = -1.5)
dsi_1<- dr_severity(severity = -1)

dsi_1_yearly = list()
for (i in 1:catch_n){
dsi_1_yearly[[i]] = dsi_1[[i]] %>% 
  mutate(year = year(dr_start)) %>% 
  group_by(year) %>% 
  summarise(mean_dsi = mean(dsi), mean_length = mean(dr_length), mean_inten = mean(dr_intens))

}




