
# drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!





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

dsi_1_yearly = list()
for (i in 1:catch_n){
dsi_1_yearly[[i]] = dsi_1[[i]] %>% 
  mutate(year = year(dr_start)) %>% 
  group_by(year) %>% 
  summarise(sum_dsi = sum(dsi), sum_length = sum(dr_length), sum_inten = sum(dr_intens), n = n())

}





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




#80th percentile approach of van loon &laaha####
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
  createlfobj(., baseflow=F, hyearstart=1) %>% 
  as.xts()
#flowunit(lf_obj)<-'m³/s' default is m³/s so no new default definition is needed

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
  createlfobj(., baseflow=F, hyearstart=1) %>% 
  as.xts()
flowunit(lf_obj)<-"l/d" #default is m³/s so new default definition is needed

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
#warings are due to assuming the default in the deficit unit (the default is correct!!) and because of Na values. NA values exist because the mean deficit is calculated from 30 day moving centered average (creating 14 NAs at the beginning at at the end of each time series)
colnames(output) = c("catchment", "dr_start", "dr_end", "def_vol", "threshhold", "mn_sm_p","event_no")
head(output)
save(output, file="./output/drought_p.Rdata")

remove(lf_obj, res,  drought_t, new.drought.no)





#80th percentile approach analysis####
load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
load("./output/drought_p.Rdata", verbose = TRUE)
drought_p = output


remove(output)

sum_q = drought_q %>% 
  as.tbl %>% 
  mutate(dr_start = ymd(dr_start), dr_end=ymd(dr_end), catchment= as.integer(catchment)) %>%   group_by(catchment, year = as.integer(year(dr_start))) %>% 
  summarise(n_events = n(), sm_length = as.numeric(sum(dr_end-dr_start)), tot_defi = as.numeric(round(sum(def_vol*(dr_end-dr_start)),0)),  mn_defi = as.numeric(round(tot_defi/n_events,0)),mn_length= round(sm_length/n_events,0)) %>% 
  mutate(mn_length = as.integer(mn_length))
#deficit vol in m³ increase per day of drought

tot_defi_catch = sum_q %>% 
  group_by(catchment) %>% 
  summarise(sm_tot_defi = sum(tot_defi)) %>% 
  mutate(hydrogeo = gauges$hydrogeo_simple)

int = which.max(tot_defi_catch$mn_tot_defi) # remove to extreme

pdf("./plots/4_choice/geo_tot_defi.pdf")
boxplot((sm_tot_defi) ~ hydrogeo, data=tot_defi_catch)#[-int,])
dev.off()
tot_defi_q= sum_q %>% 
  ungroup() %>% 
  mutate(stan_defi = (tot_defi - mean(tot_defi))/sd(tot_defi)) %>% #has to be standadized see laaha et al 2015
  dplyr::select(catchment, stan_defi, year) %>% 
  spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
  dplyr::select(-year)%>% 
  as.data.frame()

mn_defi_q= sum_q %>% 
  ungroup() %>% 
  mutate(stan_defi = (mn_defi - mean(mn_defi))/sd(mn_defi)) %>% #has to be standadized see laaha et al 2015
  dplyr::select(catchment, stan_defi, year) %>% 
  spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
  dplyr::select(-year)%>% 
  as.data.frame()

mn_length_q= sum_q %>% 
  ungroup() %>% 
  dplyr::select(catchment, mn_length, year) %>% 
  spread(., key=(catchment), value=(mn_length), fill = 0)%>% 
  dplyr::select(-year)%>% 
  as.data.frame()

sm_length_q= sum_q %>% 
  ungroup() %>% 
  dplyr::select(catchment, sm_length, year) %>% 
  spread(., key=(catchment), value=(sm_length), fill = 0)%>% 
  dplyr::select(-year)%>% 
  as.data.frame()


sum_p= drought_p %>% 
  as.tbl %>% 
  mutate(dr_start = ymd(dr_start), dr_end=ymd(dr_end), catchment= as.integer(catchment)) %>%   group_by(catchment, year = as.integer(year(dr_start))) %>% 
  summarise(n_events = n(), sm_length = as.numeric(sum(dr_end-dr_start)), tot_defi = as.numeric(round(sum(def_vol*(dr_end-dr_start)),0)),  mn_defi = as.numeric(round(tot_defi/n_events,0)),mn_length= round(sm_length/n_events,0)) %>% 
  mutate(mn_length = as.integer(mn_length))



tot_defi_p= sum_p %>% 
  ungroup() %>% 
  mutate(stan_defi = (tot_defi - mean(tot_defi))/sd(tot_defi)) %>% 
  dplyr::select(catchment, stan_defi, year) %>% 
  spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

mn_defi_p= sum_p %>% 
  ungroup() %>% 
  mutate(stan_defi = (mn_defi - mean(mn_defi))/sd(mn_defi)) %>% #has to be standadized see laaha et al 2015
  dplyr::select(catchment, stan_defi, year) %>% 
  spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
  dplyr::select(-year)%>% 
  as.data.frame()

mn_length_p= sum_p %>% 
  ungroup() %>% 
  dplyr::select(catchment, mn_length, year) %>% 
  spread(., key=(catchment), value=(mn_length), fill = 0)%>% 
  dplyr::select(-year)%>% 
  as.data.frame()

sm_length_p= sum_p %>% 
  ungroup() %>% 
  dplyr::select(catchment, sm_length, year) %>% 
  spread(., key=(catchment), value=(sm_length), fill = 0)%>% 
  dplyr::select(-year)%>% 
  as.data.frame()

