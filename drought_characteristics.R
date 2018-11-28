
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
#   
# 
# LQ <- q80_q2$mov_mn_q
# quantile(LQ, .2, na.rm=T)
# 
# LQ_sort <- sort(LQ, decreasing = T, na.last = T)
# n <- length(LQ)
# Femp <- 1:n/(n+1)
# plot(Femp, LQ_sort)
# 
# q80_q = mov_mn_q %>% 
#   mutate(date=date_seq_long) %>% 
#   gather(key=gauge, value=mov_mn_q, -date)  %>% 
#   mutate(gauge=as.numeric(gauge)) %>% 
#   mutate(num_day = yday(date)) %>% 
#   group_by(gauge, num_day) %>% 
#   summarise(q80= quantile(mov_mn_q,.2, na.rm=T)) %>% #is the equivilent of the 80th percentile of the flow duration curve
#   ungroup() %>% 
#   as.tbl()
# 
#   

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


#for precipitation (not completed)####

  mov_sm_p = rollapply(precip, width=30, by.column=T, align= "center(not", FUN=sum, fill=NA) %>% as.data.frame() %>% as.tbl()
mov_sm_p_long = mov_sm_p %>% 
  mutate(date=date_seq_long) %>% 
  gather(key=gauge, value=sum_mm, -date) %>% 
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

remove(lf_obj, res,  drought_t, new.drought.no)



