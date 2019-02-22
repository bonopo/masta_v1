
# drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!
#source("./R/masta_v1/functions.R")
#source("./R/masta_v1/clustering.R")


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
  mutate(year = year(dr_start)) %>% # defining the year of the drought beginning to the year in which the drought happened
  group_by(year) %>% 
  summarise(sum_dsi = sum(dsi), sum_length = sum(dr_length), sum_inten = sum(dr_intens), n = n())

}

dsi_0_yearly = list()
for (i in 1:catch_n){
dsi_0_yearly[[i]] = dsi_0[[i]] %>% 
  mutate(year = year(dr_start)) %>% # defining the year of the drought beginning to the year in which the drought happened
  group_by(year) %>% 
  summarise(sum_dsi = sum(dsi), sum_length = sum(dr_length), sum_inten = sum(dr_intens), n = n())

}

# sev <- lapply(dsi_0_yearly, function(x) x[2])
# severity = do.call( "cbind",sev)
# len <- lapply(dsi_0, function(x) x[1])
# severity = do.call( "cbind",len)

#drought frequency####
#severity per year
mat_dsi= matrix(0, nrow=40, ncol=catch_n)
for (i in 1:catch_n){
int = pmatch(c(dsi_1_yearly[[i]][,1])$year,c(1970:2009 ) )
  mat_dsi[int,i] = c(dsi_1_yearly[[i]][,2])$sum_dsi
}
#number of events per year
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
#visual plot checking ####
# for (i in sample(338, size=10)){
# plot(yearly_q10[,i]~summer_q10[,i], main=i)
#   Sys.sleep(1)
#   }




#80th percentile approach of van loon &laaha (calculation) ####
#through this method more droughts are found in precipitation than in streamflow
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

res= lfstat::find_droughts(lf_obj, threshold = "Q80", varying="daily") 
#daily is not really daily. It is actually the 30day moving sum calculated two steps before

#same as laaha approach saying the 80th percentile of the flow duration curve, with daily varying threshold. Comparison to own threshold calculation gives the same result see commented out part above

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


#80th percentile approach summary calculation####

#loading data from previous step
load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
load("./output/drought_p.Rdata", verbose = TRUE)
  drought_p = output
remove(output)
hydro_year
#drought frequency as events per year ####
q_drought_freq =  drought_q %>% 
  filter(catchment <= catch_n) %>% #because drought_q was calculated with the original number of observations of 338 catchments
  
  dplyr::select(catchment, dr_start, dr_end, event_no) %>% 
  as.tbl() %>% 
   mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
  mutate(mid_year = year(dr_start+floor((dr_end-dr_start)/2))) %>%  #attributing the drought to the mid year of every event
  group_by(catchment,mid_year) %>% 
  summarise(events=n()) %>% 
  spread(value=events, key=catchment) %>% 
  dplyr::select(-mid_year) %>% 
  set_colnames(1:catch_n) %>% 
  as.data.frame()

q_drought_freq[is.na(q_drought_freq)] = 0 #NA are produced in those years where there are no droughts therefore the value has to be set to 0 


# calculating result through summary (per year and month) 
# function takes a while and runs on all cores -1 (so please go get a coffee)
q_seas= seasonal_80th(data = drought_q)
p_seas= seasonal_80th(data = drought_p)

# returns list with 3 df for each catchment [[1]] = mat_days (= days of drought per year per month) [[2]] = mat_def (=deficit volume per month (of drought) per year)
  
#saving result for next step
save(q_seas,file="./output/seasonal_q.Rdata")
save(p_seas,file="./output/seasonal_p.Rdata")


# seasonal 80th % method analysis####

load("./output/seasonal_q.Rdata", verbose = T)
load("./output/seasonal_p.Rdata", verbose = T)

#yearly analysis 
#(summing all rows to make sums for every year to see changes over the year rather than changes over the seasons) 
#the result (df with catchments as columns and rows as years) for trend analysis
#matching drought characteristics to the hydrological year
# lapply(p_seas, function(x) apply(rbind(x[[1]][2:40,1:3], c(NA,NA,NA))
# p_seas[[1]]

p_days_of_drought_list = lapply(p_seas, function(x) x[[1]])
q_days_of_drought_list = lapply(q_seas, function(x) x[[1]])
p_days_of_drought_df <- lapply(p_seas, function(x) x[[1]]) %>% do.call("rbind", .) 
q_days_of_drought_df <- lapply(q_seas, function(x) x[[1]]) %>% do.call("rbind", .)
p_sum_def_list = lapply(p_seas, function(x) x[[2]])
q_sum_def_list = lapply(q_seas, function(x) x[[2]])

p_sum_def_df = lapply(p_seas, function(x) x[[2]]) %>% do.call("rbind", .)
q_sum_def_df<- lapply(q_seas, function(x) x[[2]]) %>% do.call("rbind", .)

p_n_df = lapply(p_seas, function(x) x[[3]]) %>% do.call("rbind", .)
q_n_df<- lapply(q_seas, function(x) x[[3]]) %>% do.call("rbind", .)


p_days_of_drought_yr = apply(p_days_of_drought_df,1, sum )%>% cbind(days_dr=.,year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40))%>%as.data.frame() %>%  spread(key=gauge, value = days_dr)%>% dplyr::select(-year)

q_days_of_drought_yr = apply(q_days_of_drought_df,1, sum )%>% cbind(days_dr=., year = rep(1970:2009,catch_n),gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = days_dr) %>% dplyr::select(-year)

p_sum_def_yr = apply(p_sum_def_df,1, sum ) %>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

q_sum_def_yr = apply(q_sum_def_df,1, sum )%>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

p_n_events_yr = apply(p_n_df,1, sum ) %>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)

q_n_events_yr = apply(q_n_df,1, sum )%>% cbind(sum_def=., year = rep(1970:2009,catch_n), gauge= rep(1:catch_n, each=40)) %>%as.data.frame() %>%  spread(key=gauge, value = sum_def) %>% dplyr::select(-year)


#decadal analysis####
#in the 70s there was a major drought dominating the trends (leading to mainly neg. trends since all droughts after the 70s were less severe).
p_days_of_drought_list <- lapply(p_seas, function(x) x[[1]]) 
q_days_of_drought_list <- lapply(q_seas, function(x) x[[1]]) 

p_sum_def_list = lapply(p_seas, function(x) x[[2]]) 
q_sum_def_list <- lapply(q_seas, function(x) x[[2]]) 


seasonal_dec_80_ana(data= q_days_of_drought_list) 
seasonal_dec_80_ana(data= p_days_of_drought_list)
seasonal_dec_80_ana(data= p_sum_def_list)
seasonal_dec_80_ana(data= q_sum_def_list)


#preperation of seasonal data for trend analysis

march_dy_drought_p = seasonal_80th_trend(month = 3, datax= p_days_of_drought_list)
march_dy_drought_q = seasonal_80th_trend(month = 3, datax= q_days_of_drought_list)
march_sm_def_p = seasonal_80th_trend(month = 3, datax= p_sum_def_list) 
march_sm_def_q = seasonal_80th_trend(month = 3, datax= q_sum_def_list) 


summer_dy_drought_p = seasonal_80th_trend(month = 5:11, datax= p_days_of_drought_list)
summer_dy_drought_q = seasonal_80th_trend(month = 5:11, datax= q_days_of_drought_list)
summer_sm_def_p = seasonal_80th_trend(month = 5:11, datax= p_sum_def_list) 
summer_sm_def_q = seasonal_80th_trend(month = 5:11, datax= q_sum_def_list) 

winter_dy_drought_p = seasonal_80th_trend(month = c(12,1,2,3,4), datax= p_days_of_drought_list)
winter_dy_drought_q = seasonal_80th_trend(month = c(12,1,2,3,4), datax= q_days_of_drought_list)
winter_sm_def_p = seasonal_80th_trend(month = c(12,1,2,3,4), datax= p_sum_def_list) 
winter_sm_def_q = seasonal_80th_trend(month = c(12,1,2,3,4), datax= q_sum_def_list) 

spring_dy_drought_p = seasonal_80th_trend(month = 3:5, datax= p_days_of_drought_list)
spring_dy_drought_q = seasonal_80th_trend(month = 3:5, datax= q_days_of_drought_list)
spring_sm_def_p = seasonal_80th_trend(month = 3:5, datax= p_sum_def_list) 
spring_sm_def_q = seasonal_80th_trend(month = 3:5, datax= q_sum_def_list)

for (i in 1:12){
  
  assign(paste0(str_to_lower(month.abb[i]),"_dy_drought_q"), seasonal_80th_trend(month = i, datax= q_days_of_drought_list))
  
}

for (i in 1:12){
  
  assign(paste0(str_to_lower(month.abb[i]),"_sum_drought_q"), seasonal_80th_trend(month = i, datax= q_sum_def_list))
  
}

march_dy_drought_q = seasonal_80th_trend(month = 3, datax= q_days_of_drought_list)
march_dy_drought_q = seasonal_80th_trend(month = 3, datax= q_days_of_drought_list)

march_dy_drought_q = seasonal_80th_trend(month = 3, datax= q_days_of_drought_list)
june_dy_drought_q = seasonal_80th_trend(month = 6, datax= q_days_of_drought_list)

#tot deficit
png("./plots/5_choice/boxplot_geo.png")
boxplot(log10(apply(q_sum_def_yr, 2, sum)) ~ gauges$hydrogeo_simple)
dev.off()


#drought event realtionship:streamflow vs precipitation####
#If severe drought than maybe there has been a major rainfall defcitit in the past (spi-6/-12?)
#with spi as indicator for precipitastion and ssi as indicator of drought 

spi_12_long = spi_v2_12 %>% 
  mutate(yr_mt = date_seq) %>% 
  gather(key=gauge, value=spi_12, -yr_mt) 

spi_06_long = spi_v2_6 %>%
  mutate(yr_mt= date_seq) %>% 
  gather(key=gauge, value=spi_06, -yr_mt) 
  
spi_24_long = spi_v2_24 %>% 
  gather(key=gauge, value=spi_24) 

spi_lt_long = spi_v2_3 %>% 
  mutate(yr_mt = date_seq) %>% 
  gather(key=gauge, value=spi_03, -yr_mt) %>% 
  mutate(spi_06 = spi_06_long$spi_06) %>% 
  mutate(spi_12 = spi_12_long$spi_12) %>% 
  mutate(spi_24 = spi_24_long$spi_24) %>% 
  as.tbl()

#getting the mid date of every drought event (80th method) to retrieve the equivelent spi_n value of that date. this is a crude method, alternative could be to average all spi_n values during the drought if the drought lasts longer than one month
drought_q_80 = drought_q %>% 
  mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
  mutate(intensity = mn_q/threshhold) %>% 
  mutate(mid_date = dr_start+floor((dr_end-dr_start)/2)) %>% 
  mutate(mid_mt_yr = ymd(paste0(year(mid_date),"-", month(mid_date),"-15"))) %>% 
  as.tbl() 

#severity is the sum of the ssi deviation of the threshhold (0) during the drought event
#getting the mid date of every drought event (ssi threshhold method, threshhold beeing ssi <0 = drought) to retrieve the equivelent spi_n value of that date 
drought_q_ssi= list() 
 for( i in 1:catch_n) {
    temp= dsi_1[[i]] %>% 
  mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
  mutate(mid_date = dr_start+floor((dr_end-dr_start)/2)) %>% 
  mutate(mid_mt_yr = ymd(paste0(year(mid_date),"-", month(mid_date),"-15"))) %>% 
  as.tbl()
    drought_q_ssi[[i]] = temp
    }

#same procedure but with seasonal approach not dsi method
drought_q


mat= matrix(nrow=catch_n, ncol=4)

#corelating spi with severity. Hypothesis: the higher the severity the higher the correlation with higher spi_n (ie spi_24). result is a matrix of the pearson correlation of the selected spi_n aggregation months with the severity of the drought.
for( i in 1:catch_n){
  spi_temp = filter(spi_lt_long, gauge == i)
  drought_q_t = drought_q_ssi[[i]]
  int = pmatch(drought_q_t$mid_mt_yr, spi_temp$yr_mt)
  spi_catch= spi_temp[int, 3:6]
  spi_catch_long = spi_catch %>%
        mutate(event_n = 1:nrow(spi_catch), date= drought_q_t$mid_mt_yr)  %>% 
        gather(key= spi_n, value=spi, -event_n, -date)
  
  mat[i,] = cor(method = "p", use="na.or.complete", y=drought_q_t$dsi, x=spi_catch)

  #instead of correlation looking at the abs differnce meaning durng drought event what is the spi-n  value and the spei-n value. is there a relationchip. Can the 2000 drough be explained?

#converting the correlation matrix into a df so it can be used to plot with ggplot
drought_severity_spi_cor= mat %>% 
  as.data.frame() %>% 
  mutate(gauge = 1:catch_n) %>% 
  set_colnames(., c("spi_03", "spi_06", "spi_12", "spi_24", "gauge")) %>% 
  gather(key=spi_n, value=spi, -gauge) %>% 
  as.tbl() %>% 
  mutate(bfi = rep(gauges$bfi, 4), saar= rep(gauges$saar, 4), spi_cor = rep(gauges$cor_spi_n,4), lt= rep(gauges$lt_memoryeffect, 4), cor_spi_drought = rep(gauges$cor_spi_dr,4),cor_spi_drought_n = rep(gauges$cor_spi_n_dr,4))
  
#first graphic analysis

ggplot(data = drought_severity_spi_cor)+
  geom_point(aes(x=saar, y=spi, col=spi_n),na.rm=T)+#, position = position_dodge(width=.3))+
  ylab("spi pearson cor spi_n ~ dsi")+
  xlab("saar")

ggsave("./plots/5_choice/spi_drought_saar.png")

#one really negative catchment:
 drought_severity_spi_cor[which.min(drought_severity_spi_cor$spi),]
 #catchment 338
 #caused by the method of selecting the mid-month to retrieve the spi. In this catchment there was one very long drought (1339 days) with average month:
 
 spi_v2_6$`338`[c(which(date_seq == "2000-04-15"):which(date_seq == "2003-12-15"))] %>% mean()
 
 #mean leads no better result. min?
 
  spi_v2_6$`338`[c(which(date_seq == "2000-04-15"):which(date_seq == "2003-12-15"))] %>% min()
  
  #not really better
  #this catchment is probably altered or its water was used for something else
 
 
 ggplot()+
    geom_point(aes(x= drought_q_t$mid_mt_yr, y=drought_q_t$dsi))+
    geom_point(data = spi_catch_long, aes(x=date, y=spi, col=spi_n))

#with SPEI as climate indicator
spei_12_long = spei_v2_12 %>% 
  mutate(yr_mt = date_seq) %>% 
  gather(key=gauge, value=spei_12, -yr_mt) 

spei_06_long = spei_v2_6 %>% 
  gather(key=gauge, value=spei_06) 
  
spei_24_long = spei_v2_24 %>% 
  gather(key=gauge, value=spei_24) 

spei_lt_long = spei_v2_3 %>% 
  mutate(yr_mt = date_seq) %>% 
  gather(key=gauge, value=spei_03, -yr_mt) %>% 
  mutate(spei_06 = spei_06_long$spei_06) %>% 
  mutate(spei_12 = spei_12_long$spei_12) %>% 
  mutate(spei_24 = spei_24_long$spei_24) %>% 
  as.tbl()



mat= matrix(nrow=catch_n, ncol=4)

for( i in 1:catch_n){
  spei_temp = filter(spei_lt_long, gauge == i)
  drought_q_t = drought_q_ssi[[i]]
  int = pmatch(drought_q_t$mid_mt_yr, spei_temp$yr_mt)
  spei_catch= spei_temp[int, 3:6]
  spei_catch_long = spei_catch %>%
        mutate(event_n = 1:nrow(spei_catch), date= drought_q_t$mid_mt_yr)  %>% 
        gather(key= spei_n, value=spi, -event_n, -date)
  
  mat[i,] = cor(method = "p", use="na.or.complete", y=drought_q_t$dsi, x=spei_catch)
  # print(
  #   ggplot()+
  #   geom_point(aes(x= drought_q_t$mid_mt_yr, y=drought_q_t$dsi))+
  #   geom_point(data = spi_catch_long, aes(x=date, y=spi, col=spi_n))
  #   
  #   )
  # 
    }

drought_severity_spei_cor= mat %>% 
  as.data.frame() %>% 
  mutate(gauge = 1:catch_n) %>% 
  set_colnames(., c("spei_03", "spei_06", "spei_12", "spei_24", "gauge")) %>% 
  gather(key=spei_n, value=spei, -gauge) %>% 
  as.tbl() %>% 
  mutate(bfi = rep(gauges$bfi, 4), saar= rep(gauges$saar, 4), spei_cor = rep(gauges$cor_spei_n,4), lt= rep(gauges$lt_memoryeffect, 4), cor_spei_drought = rep(gauges$cor_spei_dr,4),cor_spei_drought_n = rep(gauges$cor_spei_n_dr,4))
  
  

ggplot(data = drought_severity_spei_cor)+
  geom_point(aes(x=cor_spei_drought, y=spei, col=spei_n),na.rm=T)+#, position = position_dodge(width=.3))+
  ylab("spei pearson cor spei_n ~ dsi")+
xlab("best spei_n aggregation month considering only drought month (according to ssi<=-1)")

ggsave("./plots/5_choice/spei_drought_cor_ssi_best_month.png")


#comparing both the deficit/ days of drought / severity of every event in streamflow and precipitation. Is there a linear relationship? ####

##getting the mid date of every drought event (80th method) to retrieve the equivelent spi_n value of that date and calculating the intensity (as alternative to severity)
drought_q_80 = drought_q %>% 
  mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
  mutate(intensity = mn_q/threshhold) %>% 
  mutate(mid_date = dr_start+floor((dr_end-dr_start)/2)) %>% 
  mutate(mid_mt_yr = ymd(paste0(year(mid_date),"-", month(mid_date),"-15"))) %>% 
  as.tbl() 

drought_p_80 = drought_p %>% 
  mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
  mutate(intensity = mn_sm_p/threshhold) %>% 
  mutate(mid_date = dr_start+floor((dr_end-dr_start)/2)) %>% 
  mutate(mid_mt_yr = ymd(paste0(year(mid_date),"-", month(mid_date),"-15"))) %>% 
  as.tbl() 

#which catchments have a high correlation between spi and ssi during drought, meaning that the droughts are more precipitation (if SPI) or precipitation-evaporation controlled (if SPEI)

 catchment_drought_relation = which(gauges$cor_spi_dr>.5) 

#drawing an example plot to see if there is any realtionship (before calculating correlation)
 
 for (i in catchment_drought_relation){
   print(
     ggplot()+
     geom_line(data= drought_p_80 %>% filter(.,catchment==i),
               aes(x=mid_date, y=intensity), col=1)+
     geom_point(data= drought_q_80 %>% filter(.,catchment==i), 
               aes(x=mid_date, y=intensity), col=2)+
     xlab(i)
   )
 }
 # not really no relationship. too many precipitation events
 
 #using severity method: looking at the severity of precipitation drought events (SPI_6 as the best compromise) and and the severity of streamflow droughts
 #using the same severity threshold for both (-1)
 dsi_spi_6 =  dr_severity(datax = spi_06_long, severity = -1)
 dsi_ssi   =  dr_severity(datax = ssi_1_long, severity = -1)
 
 #calculating mid date to plot in the next step
 
 drought_p_spi_6= list() 
 for( i in 1:catch_n) {
    temp= dsi_spi_6[[i]] %>% 
    mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
    mutate(mid_date = dr_start+floor((dr_end-dr_start)/2)) %>% 
    mutate(mid_mt_yr = ymd(paste0(year(mid_date),"-", month(mid_date),"-15"))) %>% 
    as.tbl()
    drought_p_spi_6[[i]] = temp
 }
 
 drought_q_ssi= list() 
 for( i in 1:catch_n) {
    temp= dsi_ssi[[i]] %>% 
    mutate(dr_start = ymd(dr_start), dr_end = ymd(dr_end)) %>% 
    mutate(mid_date = dr_start+floor((dr_end-dr_start)/2)) %>% 
    mutate(mid_mt_yr = ymd(paste0(year(mid_date),"-", month(mid_date),"-15"))) %>% 
    as.tbl()
    drought_q_ssi[[i]] = temp
 }
 
 #plotting the catchments with highest correlation
 
 for (i in catchment_drought_relation){
  spi_dr = drought_p_spi_6[[i]]
     ssi_dr = drought_q_ssi[[i]]
    print(
        ggplot()+
     geom_line(data= spi_dr,
               aes(x=dr_start, y=dsi), col=1)+
     geom_point(data= ssi_dr, 
               aes(x=dr_end, y=dsi), col=2)+
     xlab(i)
   )
 }
 
 #problem: calculating correlation. Since the 80th method and the dsi method lead to different amount of droughts (according to their definition). This causing f.e. more doughts in precipitation in one catchment than measured in streamflow. 
 
 
 #drought charachteristics####
 plot(gauges$mn_length~ gauges$saar)
 
lm(gauges$mn_length~ gauges$saar) %>% summary
 
#with only poros aquifer? or low bfi?
gauges_df= gauges %>% as.data.frame()
lm(gauges_df$mn_intensity~ gauges_df$saar, subset=which(gauges$bfi <.5)) %>% summary()


plot(mn_length~ saar, data= gauges_df[which(gauges_df$bfi <.6),])
 
lm(gauges$mn_deficit~ gauges$saar, subset=which(gauges$hydrogeo_simple == "P")) %>% summary()
