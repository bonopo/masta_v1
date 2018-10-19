
# Clustering --------------------------------------------------------------
source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")

#seasonality ratio (SR)

# creating two time series one winter one summer
#calculating q95 for both parts
q_sr_w <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month > 11 | month <4) %>%  #definitin of winter from Laaha et al 2006
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_w = mean(qt))

q_sr_s <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month < 12 | month > 3) %>%
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_s = mean(qt))

q_sr <- merge(q_sr_s, q_sr_w, by="gauge")
q_sr$sr <- q_sr$q95_s/q_sr$q95_w # SR is caclulated via q95_summer/q95_winter from Laaha et al 2006

q_sr$sr_value[which(q_sr$sr < 1)] <- 0 #summer
q_sr$sr_value[which(q_sr$sr > 1)] <- 1 #winter NAs are produced in 9 time series that are very altered or have no clear seasonality

gauges$sr <- as.numeric(q_sr$sr_value)
#spplot(gauges, "sr")

# ind <- which(is.na(q_sr$sr_value))
# q_long %>%
#   filter(gauge %in% ind) %>%
#   filter(year(date) < 1975) %>%
#   ggplot() +
#   geom_smooth(aes(x=date, y=q, group= as.factor(gauge), color= as.factor(gauge)))+
#   scale_y_log10()

gauges$ezggr_class <- cut(gauges$Enzgsg_, breaks=c(0,50,100,150,Inf), labels=c("<50", "50-100", "100-150", "150-200"))


# BFI clustering according  ---------------------------------------------
bfi <- c()
for (i in 1:catch_n){
lf_obj <- q_long %>% 
  filter(gauge == i) %>% 
  mutate( flow= q,day = day(date), month=month(date), year = year(date)) %>% 
  dplyr::select(-date, -gauge, -q) %>% 
  as.data.frame()
 
basefl <- createlfobj(x= lf_obj, hyearstart = 1, baseflow = T)
bfi[i] <- BFI(basefl)
}
plot(bfi)
