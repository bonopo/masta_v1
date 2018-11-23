

# climate characteristics -------------------------------------------------

# source("./R/masta_v1/functions.R")# has to run before if not objects will be missing!
# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missing!

# q seasonal data ####
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

remove(ms7_df, ms7, q_wide_summer, ms7_min_temp, int)

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


#ms30_date and ms7_date do differ a lot see Plots
for (i in sample(338)){
plot(ms30_date[,i]~ms7_date[,i], main=i)
  Sys.sleep(1)
  }

remove(ms30, ms30_df, ms30_summer)

#comparing ms7 with ms30

plot(ms30_min[,11] ~ ms7_min[,11])

#summer mean q

su_mn_q = summer_cl(data_source = "mt_mn_q", method = "mean", value = "q_mean", begin =5, end=11) 
su_min_q = summer_cl(data_source = "mt_mn_q", method = "min", value = "q_mean", begin =5, end=11) #summer mnq30


su_q10 =  q_long %>% 
  filter(month(date) >= 5, month(date)<= 11) %>% 
  mutate(year=year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(q10 = quantile(q, .1)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q10) %>% 
  dplyr::select(-year ) %>% 
  as.data.frame()

# su_q = mt_mn_q %>% 
#   filter(month(yr_mt) >= 4, month(yr_mt)<= 10) %>% 
#   spread(key=gauge, value=q_mean) %>% 
#   dplyr::select(-yr_mt, -month) %>% 
#   as.data.frame()

# wi_q = mt_mn_q %>% 
#   filter(month(yr_mt) < 4 | month(yr_mt) > 10) %>%
#   spread(key=gauge, value=q_mean) %>% 
#    dplyr::select(-yr_mt, -month) %>% 
#    as.data.frame()

wi_q10 = q_long %>% 
  filter(month(date) < 5 | month(date) > 11) %>%
  mutate(year=year(date)) %>% 
  group_by(gauge, year) %>% 
  summarise(q10 = quantile(q,.1)) %>% 
  spread(key=gauge, value=q10) %>% 
   dplyr::select( -year) %>% 
   as.data.frame()




#q yearly data ####
yearly_mn_q = q_long %>% 
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





#q monthly data ####
# monthly_nq = q_long %>%  #not usable rather use mnq7
#   mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
#   group_by(gauge, yr_mt) %>% 
#   summarise(monthly_min = min(q)) %>% 
#   spread(key = gauge, value=monthly_min) %>% 
#   dplyr::select(-yr_mt) %>% 
#   as.data.frame()

# monthly_mean = q_long %>% 
#   mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
#   group_by(gauge, yr_mt) %>% 
#   summarise(monthly_mean = mean(q)) %>% 
#   spread(key = gauge, value=monthly_mean) %>% 
#   dplyr::select(-yr_mt) %>% 
#   as.data.frame()

monthly_mean_t = q_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = mean(q)) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_mean_t, c("gauge", "month"))
 int = seq(1,4045,12)
 for(i in 1:12){
 df=  Reduce(rbind,res[int]) %>% spread(key=gauge,value= monthly_mean) %>% dplyr::select(-yr_mt, -month)
 assign(paste0(str_to_lower(month.abb[i]),"_mn_q"),df)
 int = int+1
 }
remove(res, int, monthly_mean_t)



#temp monthly####

monthly_mean_t = temp_long %>% 
  mutate(yr_mt = ymd(paste0(year(date),"-",month(date),"-15"))) %>% 
  group_by(gauge, yr_mt) %>% 
  summarise(monthly_mean = mean(temp)) %>% 
  mutate(month= month(yr_mt))

 res =  plyr::dlply(monthly_mean_t, c("gauge", "month"))
 int = seq(1,4045,12)
 for(i in 1:12){
 df=  Reduce(rbind,res[int]) %>% spread(key=gauge,value= monthly_mean) %>% dplyr::select(-yr_mt, -month)
 assign(paste0(str_to_lower(month.abb[i]),"_mn_t"),df)
 int = int+1
 }
remove(res, int, monthly_mean_t)


#temp seasonal####

su_mn_t = mt_mn_temp %>% 
  filter(month(yr_mt) >= 5 & month(yr_mt) <= 11) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()

wi_mn_t = mt_mn_temp %>% 
  filter(month(yr_mt) < 5 | month(yr_mt) > 11) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()


#temp yearly####
yearly_mn_t = mt_mn_temp %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean_temp = mean(temp_m)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()



d30_mn_t = rollapply(data = da_temp_wide,FUN= mean, width=30, by.column=TRUE, fill=NA, align="center") 
d30_mn_t %<>% as.data.frame()


yearly_max_t = d30_mn_t %>% 
  mutate(date = date_seq_long) %>% 
  gather(key=gauge, value=d30_mn_t, -date) %>% 
  group_by(gauge, year(date)) %>% 
  summarise(mean_temp = max(d30_mn_t, na.rm=T)) %>% 
  spread(key=gauge, value=mean_temp) %>% 
  dplyr::select(-`year(date)`) %>% 
  as.data.frame()


remove(d30_mn_t)


# precipitation seasonal ####


su_sm_p = summer_cl(data_source = "mt_sm_p", method = "sum", value = "month_sum", begin =5, end=11)



wi_sm_p = mt_sm_p %>% 
  filter(month(yr_mt) <= 4 | month(yr_mt) >= 12) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(sum_mm = sum(month_sum)) %>% 
  spread(key=gauge, value=sum_mm) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()

# precipitation yearly ####

yearly_sm_p = mt_sm_p %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(sum_mm = sum(month_sum)) %>% 
  spread(key=gauge, value=sum_mm) %>% 
  dplyr::select(-`year(yr_mt)`) %>% 
  as.data.frame()
