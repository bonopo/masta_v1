
# drought Characteristics -------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")

source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!





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
for (i in sample(338, size=10)){
plot(yearly_q10[,i]~summer_q10[,i], main=i)
  Sys.sleep(1)
  }



