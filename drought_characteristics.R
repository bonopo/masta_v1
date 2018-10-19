
# Drought Characteristics -------------------------------------------------
source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")
#month with max drought#
#with ssi 

#with actuall flow data
year = year(date_seq) %>% list()
mt_mn_q$year <- unlist(rep(year, times = catch_n))
mnq30 <- aggregate(mt_mn_q_wide, by= year, FUN= min, by.column=T)

for ( i in 1:338){
data <- mt_mn_q %>% 
  filter(gauge == i)
data_by <- data %>% group_by(year(yr_mt)) %>% 
  summarise(mon_min = month(yr_mt[which.min(q_mean)]))  
  mnq30_month[i] <- names(which.max(table(data_by$mon_min)))
}

plot(mnq30_month)
