
# drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!
#source("./R/masta_v1/functions.R")


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

remove(data, data_by, mnq30_month)
#number of months in a year affected by drought ####


dr_length_1 <- dr_n()
dr_length_1_5 <- dr_n(severity = -1.5) #min value is -1.97

#drought severity & intensity####


#severity: sum of differences between ssi indicator and threshold, it indicates a cumulative deficiency of a drought parameter below the critical level. 
#intensity: it is the average value of a drought parameter below the critical level. It is measured as the drought severity divided by the duration.


dr_event_no_1<- dr_count(severity = -1) #returns every month affected by drought
dsi_1.5<- dr_severity(severity = -1.5) #returns all drought events by counting consecutive month affected by drought
dsi_1<- dr_severity(severity = -1)
dsi_0<- dr_severity(severity = 0)



dsi_1_yearly = list()
for (i in 1:catch_n){
dsi_1_yearly[[i]] = dsi_1[[i]] %>% 
  mutate(year = year(dr_start)) %>% 
  group_by(year) %>% 
  summarise(sum_dsi = sum(dsi), sum_length = sum(dr_length), sum_inten = sum(dr_intens), n = n())

}

dsi_0_yearly = list()
for (i in 1:catch_n){
dsi_0_yearly[[i]] = dsi_0[[i]] %>% 
  mutate(year = year(dr_start)) %>% 
  group_by(year) %>% 
  summarise(sum_dsi = sum(dsi), sum_length = sum(dr_length), sum_inten = sum(dr_intens), n = n())

}

# sev <- lapply(dsi_0_yearly, function(x) x[2])
# severity = do.call( "cbind",sev)
# len <- lapply(dsi_0, function(x) x[1])
# severity = do.call( "cbind",len)

#drought frequency####

mat_dsi= matrix(0, nrow=40, ncol=catch_n)
for (i in 1:catch_n){
int = pmatch(c(dsi_1_yearly[[i]][,1])$year,c(1970:2009 ) )
  mat_dsi[int,i] = c(dsi_1_yearly[[i]][,2])$sum_dsi
}

mat_n= matrix(0, nrow=40, ncol=catch_n)
for (i in 1:catch_n){
int = pmatch(c(dsi_1_yearly[[i]][,1])$year,c(1970:2009 ) )
  mat_n[int,i] = c(dsi_1_yearly[[i]][,5])$n
}


mat_dsi[,23] %>% plot(t="l")

#per 5 year

dr_freq_5yr = rollapply(mat_n, width=5, by= 5, FUN=sum, by.column=TRUE)
dr_dsi_5yr = rollapply(mat_dsi, width=5, by= 5, FUN=sum, by.column=TRUE)
#per decade
dr_freq_10yr = rollapply(mat_n, width=10, by= 10, FUN=sum, by.column=TRUE)
dr_dsi_10yr = rollapply(mat_dsi, width=10, by= 10, FUN=sum, by.column=TRUE)
#drought free time####
#visual plot checking ####
# for (i in sample(338, size=10)){
# plot(yearly_q10[,i]~summer_q10[,i], main=i)
#   Sys.sleep(1)
#   }




#80th percentile approach of van loon &laaha (calculation) ####
#for discharge discharge ####
mov_mn_q = rollapply(q_wide, width=30, by.column=T, align= "center", FUN=mean, fill=NA) %>% as.data.frame %>% as.tbl()
mov_mn_q_long = mov_mn_q %>% 
  mutate(date=date_seq_long) %>% 
  gather(key=gauge, value=mov_mn_q, -date) %>% 
  mutate(gauge = as.numeric(gauge)) %>% 
  as.tbl()

output = matrix(nrow=0, ncol=7) %>% as.data.frame()
for (c in 1:catch_n){ 
lf_obj <- mov_mn_q_long %>% 
  filter(gauge == c) %>% 
  mutate( flow= mov_mn_q,day = day(date), month=month(date), year = year(date)) %>% 
  dplyr::select(-date, -gauge, -mov_mn_q) %>% 
  createlfobj(., baseflow=F, hyearstart=1) 

flowunit(lf_obj)<-'m³/s' 

res= find_droughts(lf_obj, threshold = "Q80", varying="daily") #same as laaha approach saying the 80th percentile of the flow duration curve, with daily varying threshold. Comparison to own threshold calculation gives the same result see commented out part above

#problem: droughts of less than 4 days are still defined as drought: But I am intested in droughts that have a long lasting effect with it's deficit in water

for (i in 1:max(res$event.no)){
  if(length(which(res$event.no == i)) <= 3 ){ #removing droughts of less than 4 days
    res$event.no[res$event.no == i] = 0
  }  
}
new.drought.no = 1:(length(unique(res$event.no))-1) #because 0 is not an event -1
n=1

for (i in unique(res$event.no)[-1]){#because 0 is not an event -1
  res$event.no[res$event.no == i] = new.drought.no[n]
  n=n+1
}

#creating a matrix with drought event results
res %<>% as.data.frame()
drought_t = matrix(nrow = max(res$event.no), ncol=7) %>% as.data.frame()
drought_t[,1]=as.numeric(c)
for (i in 1:max(res$event.no)){
drought_t[i,2]= rownames(res)[res$event.no == i][1] #drought start
drought_t[i,3]=tail(rownames(res)[res$event.no == i], n=1) #drought end
drought_t[i,4] = sum(res$def.increase[res$event.no == i]) # deficit vol
drought_t[i,5] = mean(res$threshold[res$event.no == i]) #mean threshhold
drought_t[i,6] = mean(res$discharge[res$event.no == i], na.rm=T) #mean disscharge
drought_t[i,7] = i #event no
}

output = rbind(output, drought_t)

cat(100*round(c/catch_n,2),"%", "\n")

}
#warings are due to assuming the default in the deficit unit (default is correct) and because of Na valus that exist because the mean deficit is calculated from 30 day moving centere average
colnames(output) = c("catchment", "dr_start", "dr_end", "def_vol", "threshhold", "mn_q","event_no")
head(output)
save(output, file="./output/drought_q.Rdata")

remove(lf_obj, res,  drought_t, new.drought.no, mov_mn_q, mov_mn_q_long)


#for precipitation ####

  mov_sm_p = rollapply(precip, width=30, by.column=T, align= "center", FUN=sum, fill=NA) %>% as.data.frame() %>% as.tbl()
mov_sm_p_long = mov_sm_p %>% 
  mutate(date=date_seq_long) %>% 
  gather(key=gauge, value=sum_mm, -date) %>% 
  mutate(gauge = as.numeric(gauge)) %>% 
  as.tbl()



output = matrix(nrow=0, ncol=7) %>% as.data.frame()
for (c in 1:catch_n){ 
lf_obj <- mov_sm_p_long %>% 
  filter(gauge == c) %>% 
  mutate( flow= sum_mm,day = day(date), month=month(date), year = year(date)) %>% 
  dplyr::select(-date, -gauge, -sum_mm) %>% 
  createlfobj(., baseflow=F, hyearstart=1) 
  
flowunit(lf_obj)<-"l/d" #default is m³/s so new default definition is needed. since unit is mm/day it can be set to l/d

res= lfstat::find_droughts(lf_obj, threshold = "Q80", varying="daily") #same as laaha approach saying the 80th percentile of the flow duration curve, with daily varying threshold. Comparison to own threshold calculation gives the same result see commented out part above

#problem: droughts of less than 4 days are still defined as drought: But I am intested in droughts that have a long lasting effect with it's deficit in water

for (i in 1:max(res$event.no)){
  if(length(which(res$event.no == i)) <= 3 ){ #removing droughts of less than 4 days
    res$event.no[res$event.no == i] = 0
  }  
}
new.drought.no = 1:(length(unique(res$event.no))-1) #because 0 is not an event -1
n=1

for (i in unique(res$event.no)[-1]){#because 0 is not an event -1
  res$event.no[res$event.no == i] = new.drought.no[n]
  n=n+1
}

#creating a matrix with drought event results
res %<>% as.data.frame()
drought_t = matrix(nrow = max(res$event.no), ncol=7) %>% as.data.frame()
drought_t[,1]=as.numeric(c)
for (i in 1:max(res$event.no)){
drought_t[i,2]= rownames(res)[res$event.no == i][1] #drought start
drought_t[i,3]=tail(rownames(res)[res$event.no == i], n=1) #drought end
drought_t[i,4] = sum(res$def.increase[res$event.no == i]) # deficit vol
drought_t[i,5] = mean(res$threshold[res$event.no == i]) #mean threshhold
drought_t[i,6] = mean(res$discharge[res$event.no == i], na.rm=T) #mean disscharge
drought_t[i,7] = i #event no
}

output = rbind(output, drought_t)

cat(100*round(c/catch_n,3),"%", "\n")

}
#warings are due to Na values. NA values exist because the mean deficit is calculated from 30 day moving centered average (creating 14 NAs at the beginning at at the end of each time series).

colnames(output) = c("catchment", "dr_start", "dr_end", "def_vol", "threshhold", "mn_sm_p","event_no")

save(output, file="./output/drought_p.Rdata")

remove(lf_obj, res,  drought_t, new.drought.no)


#80th percentile approach (analysis)####

load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
load("./output/drought_p.Rdata", verbose = TRUE)
  drought_p = output
remove(output)

  

  q_seas= seasonal_80th(data = drought_q)
  p_seas= seasonal_80th(data = drought_p)
  

 #result of funtion is a list: 338 list elements with two matrix per catchment [[1]]=deficit vol sum (!) per catchment per month over all 40 years!!  [[2]] sum of days per month that are effected by drought
 
save(q_seas,file="./output/seasonal_q.Rdata")
save(p_seas,file="./output/seasonal_p.Rdata")
# seasonal 80th % method (analysis)####

load("./output/seasonal_q.Rdata", verbose = T)
load("./output/seasonal_p.Rdata", verbose = T)

p_days_of_drought_df <- lapply(p_seas, function(x) x[[1]]) %>% do.call("rbind", .)
q_days_of_drought_df <- lapply(q_seas, function(x) x[[1]]) %>% do.call("rbind", .)

p_sum_def_df = lapply(p_seas, function(x) x[[2]]) %>% do.call("rbind", .)
q_sum_def_df<- lapply(q_seas, function(x) x[[2]]) %>% do.call("rbind", .)

boxplot(q_days_of_drought_df)

mt_mn_def = q_seas[[1]] %>%
  as.data.frame() %>% 
  set_colnames(1:catch_n) %>% 
  mutate(month= as.integer(rownames(.))) %>% 
  gather(key = gauge, value=mt_mn_def, -month) %>% 
  group_by(gauge) %>% # to standartize per catchment
  mutate(stan_defi = (mt_mn_def - mean(mt_mn_def))/sd(mt_mn_def)) %>%   #has to be standartized see laaha et al 2015
  ungroup() %>% 
  mutate(gauge= as.integer(gauge)) %>% 
  as.tbl()

mt_sm_events = q_seas[[2]]  %>%
  as.data.frame() %>% 
  set_colnames(1:catch_n) %>% 
  mutate(month= as.integer(rownames(.))) %>% 
  gather(key = gauge, value=mt_sm_events, -month) %>% 
  group_by(gauge) %>% # to standartize per catchment
  mutate(stan_events = (mt_sm_events - mean(mt_sm_events))/sd(mt_sm_events)) %>%   #has to be standartized see laaha et al 2015
  ungroup() %>% 
  mutate(gauge= as.integer(gauge) , mt_sm_events = as.integer(mt_sm_events)) %>% 
  as.tbl()

mt_perc_days = q_seas[[3]]  %>%
  as.data.frame() %>% 
  set_colnames(1:catch_n) %>% 
  mutate(month= as.integer(rownames(.))) %>% 
  gather(key = gauge, value=tot_days, -month) %>% 
  mutate(perc_days = round(tot_days /(40 *days_in_month(month)),3) *100) %>%  # 40 because 40 years
  as.tbl()



#%>% filter(gauge == which(gauges$alpine == 0))
ggplot()+
  geom_boxplot(data= mt_mn_def , aes(x=as.factor(month), y=stan_defi, group = month),stat="boxplot" )+
  xlab("Month")+
  ylab("standardized deficit vol. during droughts [all catchments]")


ggplot()+
  geom_boxplot(data= mt_mn_def , aes(x=as.factor(month), y=mt_mn_def, group = month),stat="boxplot" )+
  xlab("Month")+
  ylab("standardized deficit vol. during droughts [all catchments]")


ggplot()+
  geom_boxplot(data= mt_sm_events, aes(x=as.factor(month), y=stan_events, group = month),stat="boxplot" )+
  xlab("Month")+
  ylab("standardized deficit vol. during droughts [all catchments]")

# ggplot()+
#   geom_boxplot(data= mt_perc_days, aes(x=as.factor(month), y=perc_days, group = month),stat="boxplot" )+
#   ylim(c(0,25))+
#   xlab("Month")+
#   ylab("standardized deficit vol. during droughts [all catchments]")


#yearly q80 calculation (for yearly trend analysis)####


p_days_of_drought_df <- lapply(p_seas, function(x) x[[1]]) %>% do.call("rbind", .)
q_days_of_drought_df <- lapply(q_seas, function(x) x[[1]]) %>% do.call("rbind", .)

p_sum_def_df = lapply(p_seas, function(x) x[[2]]) %>% do.call("rbind", .)
q_sum_def_df<- lapply(q_seas, function(x) x[[2]]) %>% do.call("rbind", .)

p_days_of_drought_yr = apply(p_days_of_drought_df,1, sum )%>% cbind(days_dr=.,year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40))%>%as.data.frame() %>%  spread(key=gauge, value = days_dr)%>% dplyr::select(-year)

q_days_of_drought_yr = apply(q_days_of_drought_df,1, sum )%>% cbind(days_dr=., year = rep(1970:2009,catch_n),gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = days_dr) %>% dplyr::select(-year)

p_sum_def_yr = apply(p_sum_def_df,1, sum ) %>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

q_sum_def_yr = apply(q_sum_def_df,1, sum )%>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

q_yearly[[1]] %>% 
  mutate(year = c(1970:2009)) %>% 
  gather(key= gauge, value=def_vol, -year) %>% 
  group_by()

plot(q_sum_def_yr[,152], type="l")
