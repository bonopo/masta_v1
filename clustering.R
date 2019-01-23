
# Clustering --------------------------------------------------------------
# source("./R/masta_v1/functions.R")# has to run before if not objects will be missin!
# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!
# source("./R/masta_v1/sci_calculation.R")# has to run before if not objects will be missin!
# source("./R/masta_v1/drought_characteristics.R") # has to run before if not objects will be missing!
#source("./R/masta_v1/climate_characteristics.R") # has to run before if not objects will be missing!

#SAAR ####
#standart climate period 1971 bis 2000 (see DWD)
# standart period averae annual rainfall
saar <- precip_long %>% 
  filter(year(date) >1970 & year(date) < 2001) %>% 
  group_by(gauge) %>% 
  summarise(sum_mm_yr = sum(sum_mm)/30)

gauges$saar <- saar$sum_mm_yr
remove(saar)


#seasonality ratio (SR)####

# creating two time series one winter one summer
#calculating q95 for both parts
q_sr_w <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month > 11 | month <5) %>%  #changed definitin of winter from Laaha et al 2006 to stahl so it is consistent for all!
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_w = mean(qt))

q_sr_s <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month < 12 & month > 4) %>%
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_s = mean(qt))

q_sr <- merge(q_sr_s, q_sr_w, by="gauge")
q_sr$sr <- q_sr$q95_s/q_sr$q95_w # SR is caclulated via q95_summer/q95_winter from Laaha et al 2006

q_sr$sr_value = NA
q_sr$sr_value[which(q_sr$sr < .9)] <- 0 #summer
q_sr$sr_value[which(q_sr$sr > 1.1)] <- 2 #winter NAs are produced in 9 time series that are very altered or have no clear seasonality
q_sr$sr_value[which(q_sr$sr >= .9 & q_sr$sr <= 1.1)] = 1 #no clear seasonality 

q_sr$sr_value_new = NA
q_sr$sr_value_new[which(q_sr$sr <= 1)] <- 0 #summer
q_sr$sr_value_new[which(q_sr$sr > 1)] <- 2 #winter NAs are produced in 9 time series that are very altered or have no clear seasonality

gauges$sr <- as.numeric(q_sr$sr_value) # 0 = summer, 1= no clear seasonality 2=winter
gauges$sr_new <- as.numeric(q_sr$sr_value_new) # 2= winter 12- 0= summer low flow

remove(q_sr, q_sr_s, q_sr_w)
#spplot(gauges, "sr")



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
gauges$bfi <- bfi
remove(bfi, lf_obj, basefl)
gauges$bfi_class = cut(gauges$bfi, breaks=c(0,.4,.6,.8,1), labels=c("<.4", ".4-.6", ".6-.8", ".8-1"))

which(gauges$bfi_class == ".8-1") %>% length()


#month with max drought overall####



mnq30_month <- c()
for ( i in 1:catch_n){
data <- mt_mn_q %>% 
  filter(gauge == i)
data_by <- data %>% group_by(year(yr_mt)) %>% 
  summarise(mon_min = month(yr_mt[which.min(q_mean)]))  
  mnq30_month[i] <- names(which.max(table(data_by$mon_min))) %>% as.integer()
}


gauges$mnq30_month = mnq30_month

gauges$mnq30_month[gauges$sr_new == 2]
remove(data, data_by, mnq30_month)
#mean q####
mean_q = apply(q_wide, 2, mean)
gauges$mn_q = mean_q
#total number of events####

n_events = c()
  for (g in 1:catch_n){
    n_events[g] = dsi_1[[g]]$event_n %>% max()
  }

gauges$n_events = n_events
remove(n_events)
#alpine rivers####
#one can see big gap between all winter catchments: further definition with catchments with higher precipitation than 1200mm (alpine vs Harz/Blackforest)

gauges$alpine = 0
gauges$alpine[which(gauges$sr_new==2)] = 1
#after visual check removing following catchments: 221 238 42 305
gauges$alpine[c(42,221,238,305)] = 0
#spplot(gauges, "alpine", identify=T)

#longterm (lt) memory effect of catchments####
#defining long term memory as the spearman correlation between ssi and spi_12
lt_cor_spi = cor_sci_ssi(sci_n = c(12), sci="spi_v2_", cor_met = "s", ssi="ssi_1")


gauges$lt_memoryeffect = lt_cor_spi[,1]



#corellation SSI (during drought) with SPI/SPEI-n ####
 #-----> see drought attribution script

#which best spi-n aggregation month and correlation value ####
  #this calculates the best spi-n aggregation month for every catchment individually and the cor value itself too. The correlation is calculated between all ssi values (not only drought) and the spi-n value. In the next step the best one is selected and stored in the gauges attribution as a characteristic describing the catchment. All ssi (and not only the ssi values that are negative; indicating drought) are considered in this step because it is an attribute describing the catchment. Spearman correlation is used (because I am interested in the rank based corelation and the actual values)
cor_spi_ssi_v2 = cor_sci_ssi(sci_n= c(1,2,3,6,12,24), cor_met="s", sci="spi_v2_", ssi="ssi_1")#correlation:  spi~ ssi
cor_spei_ssi_v2 = cor_sci_ssi(sci_n= c(1,2,3,6,12,24), cor_met="s", sci="spei_v2_", ssi="ssi_1") # correlation: spei~ssi


best_spi = c()
  value_spi = c()
best_spei = c()
  value_spei = c()

for(r in 1:catch_n){
 best_spi[r] = cor_spi_ssi_v2[r,] %>% which.max() #%>% agg_month[.]
 value_spi[r] = cor_spi_ssi_v2[r,] %>% max()}

for(r in 1:catch_n){
 best_spei[r] = cor_spei_ssi_v2[r,] %>% which.max()#%>% agg_month[.]
 value_spei[r] = cor_spei_ssi_v2[r,] %>% max()}

gauges$cor_spei_n = best_spei
gauges$cor_spi_n = best_spi
gauges$cor_spi = value_spi
gauges$cor_spei = value_spei

remove(value_spei, value_spi, best_spei, best_spi, cor_spei_ssi_v2,cor_spi_ssi_v2)

#mean deficit per drought event as clusterin method of the catchments ####
load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
mean_intensity =c()
mean_deficit =c()
mean_length = c()
n_events=c()
for (i in 1:catch_n){
  temp1 =  drought_q %>% 
    filter(catchment== i)
  mean_intensity[i] = mean(temp1$threshhold - temp1$mn_q) #deviation of the mean discharge during the drought event and the varying threshhold that defines the drought (80th percentile method van Loon 2015)
  mean_length[i] = mean(as.numeric(ymd(temp1$dr_end) - ymd(temp1$dr_start))) %>% round(.,0) #mean drought length similar to van Loon 2015
  mean_deficit[i] = mean(as.numeric(ymd(temp1$dr_end) - ymd(temp1$dr_start))*temp1$def_vol) #same calculation method as van Loon 2015 # mean(number of days * deficit)
  n_events[i]= nrow(temp1)
  
}

hist(log(mean_deficit))


#gauges$mean_deficit_overall = log(mean_deficit_per_gauge)
gauges$mn_deficit = log(mean_deficit)
gauges$mn_length = mean_length
gauges$mn_intensity = mean_intensity
gauges$n_events80 = n_events
#gauges$mean_deficit_class = base::cut(log(mean_deficit), breaks=c(11,14,17,20) )
#gauges$mean_deficit_overall_class = base::cut(gauges$mean_deficit_overall, breaks=c(7,9,11,13,15) )
remove(mean_deficit,mean_length, mean_intensity,temp1, drought_q, output, n_events)


save(gauges, file="./output/gauges.Rdata")
# End of clustering -------------------------------------------------------




#do the catchments with high bfi have longer droughts? and higher deficits? definig bfi > 0.75  (see: hist(gauges$bfi))####
# length of droughts is the same for every catchment per definition since, per definition everything below the 20th quantile counts as drought

high_bfi = which(gauges$bfi > .75)
tot_deficit = q_sum_def_yr %>% apply(., 2, sum) 
mean_deficit = q_sum_def_yr %>% apply(., 2, mean) 

boxplot( tot_deficit ~gauges$hydrogeo_simple)

ggplot()+
  geom_boxplot( aes(y=tot_deficit, x=gauges$hydrogeo_simple, col=gauges$bfi_class), position = "dodge")+
  xlab("Hydrogeology")+
  ylab("cumulative discharge deficit during drought [m³]")+
  scale_color_discrete("BFI",labels = c("<0.4", "0.4-0.6", "0.6-0.8", ">0.8"))

ggsave("./plots/clustering/hydrogeo_deficit.pdf")


ggplot()+
  geom_boxplot( aes(y=mmky_ms7_min$sen_slope[mmky_ms7_min$new_p < 0.05], x=gauges$hydrogeo_simple[mmky_ms7_min$new_p < 0.05], col=gauges$bfi_class[mmky_ms7_min$new_p < 0.05]), position = "dodge")+
  xlab("Hydrogeology")+
  ylab("ms7 trend (slope) [m³/s/a]")+
  # annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(mmky_ms7_min$new_p < 0.05))))+
  # annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label="p = 0.05")+
  scale_color_discrete("BFI",labels = c("<0.4", "0.4-0.6", "0.6-0.8", ">0.8"))

temp_df = cbind.data.frame(as.numeric(mmky_ms7_min$sen_slope[mmky_ms7_min$new_p < 0.05]), gauges$hydrogeo_simple[mmky_ms7_min$new_p < 0.05],gauges$bfi_class[mmky_ms7_min$new_p < 0.05])
ggplot()+
  geom_boxplot()+
  stat_summary(
    fun.data = stat_box_data, 
    geom = "text", 
    hjust = 0.5,
    vjust = 0.9
  )
  
ggsave("./plots/clustering/hydrogeo_ms7_min.pdf")


#comparison between ssi drought definition and 80th method drought definition####
#ssi method: discharge below ssi > -1 is equal to drought. intensity, severity and drought length rely on deviation of the SSI value of that month. Leading to droughts that always last at least one month
#80th method: 30 day moving average deviation of the 20th quantile = drought

plot(gauges$n_events ~ n_events)


#clustered plots #### 
#see barker et al

# gauges_df = as.data.frame(gauges)
# 
# head(gauges_df)
# 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=saar, y= n_events, alpha= bfi))
# 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=saar, y= best_spi, alpha= bfi))
# 
 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=saar, y= best_spei, col= bfi))
# 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=saar, y= cor_spi, alpha= bfi))
# ggsave("best_spi_cor_spi_saar.png")
# 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=saar, y= cor_spei, col= bfi))
# ggsave("bfi_cor_spei_saar.png")
# ggplot(data=gauges_df)+
#   geom_point(aes(x=Enzgsg_, y= med_dr_sev, col= bfi))
# 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=max_dr_sev, y= n_events, col= bfi))
# 
# ggplot(data=gauges_df)+
#   geom_point(aes(x=n_events, y= max_dr_dur, col = bfi))
# ggsave("max_dr_dur_med_dr_int_bfi.png")
# 
gauges$ms30 = mmky_ms30_min$sen_slope
gauges$ms7_date = mmky_ms7_date$sen_slope
min(mmky_ms7_date$sen_slope)
rtb = c('#ca0020','#f4a582','#bababa','#92c5de','#0571b0')
germany = raster::getData("GADM",country="Germany",level=0)

png("./plots/5_choice/gauges_msdate.png", width=800, height=1000)
#pdf("./plots/5_choice/gauges_msdate.pdf")
spplot(gauges, "ms7_date", 
       col.regions = rtb,
       cuts = c(-2.5,-1.5,-.5,.5,1.5,2.5),
       legendEntries = c("<-1.5", "<-0.5", "near no trend", ">0.5",">1.5"),
       sp.layout = germany,
       colorkey=T,
       scales = list(draw = TRUE))
dev.off()

tofy = c("#9ecae1","#fdae61","#d7191c","#9ecae1")
pdf("./plots/5_choice/gauges_mnq30.pdf")
spplot(gauges, "mnq30_month", 
      col.regions = tofy,
       cuts = c(1,5,8,11,12),
       #legendEntries = c("<-1.5", "<-0.5", "near no trend", ">0.5",">1.5"),
       sp.layout = germany,
       colorkey=T,
       scales = list(draw = TRUE))
dev.off()

# gauges$cor_spi_n
# 
# gauges$sr



