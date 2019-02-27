
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


#mean t####
mean_t = mt_mn_temp %>% 
  filter(year(yr_mt) >1970 & year(yr_mt) < 2001) %>% 
  group_by(gauge) %>% 
  summarise(mn_t = mean(temp_m))

gauges$mn_t = mean_t$mn_t
remove(mean_t)
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
n_summer= length(which(gauges$sr_new == 0)) 
n_winter= length(which(gauges$sr_new == 2))
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
drought_sci = dr_corr(threshhold = -1)
drought_sci_0 = dr_corr(threshhold = 0)


#correlation of ssi-1 with spi-/spei-n in drought periods####

cor_spi = matrix(nrow=catch_n, ncol=length(drought_sci_0))
cor_spei = matrix(nrow=catch_n, ncol=length(drought_sci_0))

for (a in 1:length(drought_sci_0)){
for (g in 1:catch_n){
temp= drought_sci_0[[a]] %>% 
  filter(gauge== g)
cor_spi[g, a] = cor(y= temp$ssi , x= temp$spi, use="c", method = "spearman") 
cor_spei[g, a] = cor(y= temp$ssi , x= temp$spei, use="c", method = "spearman")
}
}




#which aggregation month describes the catchment the best and what is its correlation (pearson)
best_spi = c()
  value_spi = c()
best_spei = c()
  value_spei = c()

for(r in 1:catch_n){
 best_spi[r] = cor_spi[r,] %>% which.max() 
 value_spi[r] = cor_spi[r,] %>% max()}

for(r in 1:catch_n){
 best_spei[r] = cor_spei[r,] %>% which.max() 
 value_spei[r] = cor_spei[r,] %>% max()}

gauges$cor_spei_n_dr = best_spei
gauges$cor_spi_n_dr  = best_spi
gauges$cor_spi_dr    = value_spi
gauges$cor_spei_dr   = value_spei

hist(gauges$cor_spi_n_dr)
boxplot(cor_spi)
boxplot(cor_spei)

drought_sci_0[[3]] 
  


#which best spi-n aggregation month and correlation value ####
  #this calculates the best spi-n aggregation month for every catchment individually and the cor value itself too. The correlation is calculated between all ssi values (not only drought) and the spi-n value. In the next step the best one is selected and stored in the gauges attribution as a characteristic describing the catchment. All ssi (and not only the ssi values that are negative; indicating drought) are considered in this step because it is an attribute describing the catchment. Spearman correlation is used (because I am interested in the rank based corelation and the actual values)
cor_spi_ssi_v2 = cor_sci_ssi(sci_n= c(1,3,6,12), cor_met="s", sci="spi_v2_", ssi="ssi_1")#correlation:  spi~ ssi
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

plot( x= gauges$saar, y=value_spi)
spplot(gauges, "cor_spi_n")


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
plot(gauges$n_events80~gauges$bfi)

#gauges$mean_deficit_overall = log(mean_deficit_per_gauge)
gauges$mn_deficit = log(mean_deficit)
gauges$mn_length = mean_length
gauges$mn_intensity = mean_intensity
gauges$n_events80 = n_events
#gauges$mean_deficit_class = base::cut(log(mean_deficit), breaks=c(11,14,17,20) )
#gauges$mean_deficit_overall_class = base::cut(gauges$mean_deficit_overall, breaks=c(7,9,11,13,15) )
remove(mean_deficit,mean_length, mean_intensity,temp1, drought_q, output, n_events)
plot(gauges$mn_deficit ~ gauges$mn_q)

save(gauges, file="./output/gauges.Rdata")
# End of clustering -------------------------------------------------------

#clustering analysis####
clust_ana3 = cbind( gauges$alpine, gauges$sr_new, as.factor(gauges$hydrogeo_simple), as.factor(gauges$landuse), gauges$mn_q, gauges$saar, gauges$bfi, gauges$Enzgsg_, gauges$mn_t, gauges$lt_memoryeffect,gauges$mn_deficit, gauges$mn_intensity, gauges$mn_length, gauges$cor_spei, gauges$cor_spi, gauges$cor_spei_n, gauges$cor_spi_n, gauges$Hochwrt ) %>% as.data.frame()

colnames(clust_ana3) =c("alpine",  "seasonality","hydro_geo","landuse","q_mean","saar","BFI","catchment_km", "mn_t", "memory_effect", "mn_deficit","mn_intensity","mn_length","cor_spei","cor_spi","cor_spei_n","cor_spi_n", "hochwert")
#library(scales)
# pdf("./plots/clustering/varclus.pdf")
# plot(Hmisc::varclus(~., data=clust_ana3), las=1, cex.lab=1.5)
# abline(h=.5, lty=2)
# dev.off()
cor_mat = round(cor(x= ms7_date_trend , y= clust_ana5, use="na.or.complete", method="s"),2)


plot(gauges$mn_t ~ gauges$Hochwrt)
plot(gauges$saar ~ gauges$Hochwrt)
plot(gauges$bfi~ gauges$mn_length)
plot(gauges$bfi~ gauges$mn_length)
#latitude has high cor with saar and mn_t
#alpine and sr_ew 
#mn_intensity is highly cor. with mean discharge

#throw out: Alpine, mn_intensity, latitude, mn_length, mn_deficit
clust_ana4 = dplyr::select(clust_ana3,-hochwert,-alpine,-mn_intensity, -mn_length, -mn_deficit)#, -landuse)
plot(Hmisc::varclus(~., data=clust_ana4), las=1, cex.lab=1.5)
abline(h=.5, lty=2)
#still some are too highly corrilineated
plot(x=gauges$mn_q ,y=gauges$mn_deficit)

fm = glm(mmky_ms7_min$sen_slope ~ ., data= clust_ana4)
summary(fm)
car::vif(fm)

#choosing between spi or spei??
plot(y=mmky_ms30_min$sen_slope , x = gauges$cor_spei_n)
#are both very bad throw out

install.packages("relaimpo")
trend_chara = cbind.data.frame(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p<0.05], clust_ana5[mmky_ms30_min$new_p<0.05,])
rel_impo_gauges = relaimpo::calc.relimp(trend_chara, rela=F, type="lmg")
plot(rel_impo_gauges)

fm = glm(mmky_ms30_min$sen_slope ~ ., data= clust_ana4)
summary(fm)
rel_impo_gauges = relaimpo::calc.relimp(fm)

plot(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p<0.05] ~ gauges$mn_q[mmky_ms30_min$new_p < 0.05])
#removing landuse because ther are NAs and landuse is not explainatory
ggplot()+
geom_point(aes(y= mmky_ms30_min$sen_slope, x= gauges$landuse))
plot(pca)
biplot(pca)
round(pca$rotation,2)




#stepwise regression####
clust_ana5 = dplyr::select(clust_ana4,-cor_spi, -cor_spi_n)
#removing the NAs from landuse
clust_ana5 = clust_ana5[-c(24,26,270),]
ms30_trend =mmky_ms30_min$sen_slope[-c(24,26,270)]
ms7_trend =mmky_ms7_min$sen_slope[-c(24,26,270)]
ms7_date_trend =mmky_ms7_date$sen_slope[-c(24,26,270)]
fm = step(lm(ms30_trend[mmky_ms30_min$new_p < 0.05] ~., data=clust_ana5[mmky_ms30_min$new_p < 0.05,]), direction = "both", k= log(catch_n)) #calculating bic as model comparison, 
#stepwise regression with spei best model bic:-1911.75,. spei cor is in the best model +  q_mean , + catchment_km , +memoryeffect

summary(fm)
clust_ana5 = dplyr::select(clust_ana4)
clust_ana5 = clust_ana5[-c(24,26,270),]
fm2 = step(lm(ms30_trend[mmky_ms30_min$new_p < 0.05] ~., data=clust_ana5[mmky_ms30_min$new_p < 0.05,]), direction = "both", k= log(catch_n))
#best model is seasonality , q mean, catchment size and memory effect but not cor spi. bic:-1907.77
relaimpo::calc.relimp(fm) %>% plot()
relaimpo::calc.relimp(fm2) %>% plot()
#is spei really the better predictor ? my intuition would way I made a mistake. 
#This can not be true. In any ways the models are very bad. 

#normalizing data 
clust_ana5 = clust_ana4[-c(24,26,270),]
clust_ana5$q_mean %>%  log10 %>% hist()
clust_ana5$saar %>%log10 %>% hist()
clust_ana5$BFI  %>%  hist()
clust_ana5$catchment_km %>% sqrt %>%  hist()
clust_ana5$mn_t %>% hist
clust_ana5$memory_effect %>%sqrt %>%  hist()
clust_ana6$`gauges$cor_spei[-c(24, 26, 270)]`%>%     hist
clust_ana5$cor_spi_n %>% hist

clust_ana5$q_mean = clust_ana5$q_mean %>%log10 
clust_ana5$saar =clust_ana5$saar %>%log10
clust_ana5$catchment_km = clust_ana5$catchment_km %>% sqrt
clust_ana5$memory_effect  = clust_ana5$memory_effect%>%sqrt 
#with interaction
#without spei and spi correlation
clust_ana5<-na.omit(clust_ana5)
clust_ana6 = dplyr::select(clust_ana5, -cor_spi, -cor_spi_n, -cor_spei, -cor_spei_n)
#ms30
init_mod = lm(ms30_trend[mmky_ms30_min$new_p < fs_ms30] ~., data=clust_ana6[mmky_ms30_min$new_p < fs_ms30,])
#ms7 timing
init_mod = lm(ms7_date_trend[mmky_ms7_date$new_p < fs_ms7_date] ~., data=clust_ana6[mmky_ms7_date$new_p < fs_ms7_date,])
#ms7
init_mod = lm(ms7_trend[mmky_ms7_min$new_p < fs_ms7] ~., data=clust_ana6[mmky_ms7_min$new_p < fs_ms7,])
#step
fm = step(init_mod,  direction = "both")#scope= . ~ .^2,
summary(fm)
#with spei
clust_ana6 = dplyr::select(clust_ana5, -cor_spi, -cor_spi_n)
init_mod = lm(ms7_trend[mmky_ms7_min$new_p < fs_ms7] ~., data=clust_ana6[mmky_ms7_min$new_p < fs_ms7,])
fm = step(init_mod,  direction = "forward")#scope= . ~ .^2,
summary(fm)
#with spi
clust_ana6 = dplyr::select(clust_ana5, -cor_spei, -cor_spei_n)
init_mod = lm(ms7_trend[mmky_ms7_min$new_p < fs_ms7] ~., data=clust_ana6[mmky_ms7_min$new_p < fs_ms7,])
fm = step(init_mod,  direction = "both")#scope= . ~ .^2,
summary(fm)
#with spei and spi
clust_ana6 = clust_ana5
init_mod = lm(ms7_trend[mmky_ms7_min$new_p < fs_ms7] ~., data=clust_ana6[mmky_ms7_min$new_p < fs_ms7,])
fm = step(init_mod,  direction = "forward")#scope= . ~ .^2 interaction
summary(fm)


#weird result: too many significant predictors. Model overfitted



#the lower the bic the better the model, but all models are quite bad see relaimpo output

# how much of the variance of the summer low flow trends can be explained by the catchment properties####

#pca
#only continues variables
pca <- prcomp(na.omit(clust_ana6[,c(4:9)]), scale=T) 
names(pca$sdev) = abbreviate(colnames(clust_ana6[,c(4:9)]))
clust_ana5$catchment_km
summary(pca)

png("./plots/clustering/screeplot.png", width=1000, height=750)
screeplot(pca)
dev.off()
round(pca$rotation,2)
#do the catchments with high bfi have longer droughts? and higher deficits? definig bfi > 0.75  (see: hist(gauges$bfi))####
# length of droughts is the same for every catchment per definition since, per definition everything below the 20th quantile counts as drought

high_bfi = which(gauges$bfi > .75)
tot_deficit = q_sum_def_yr %>% apply(., 2, sum) 
mean_deficit = q_sum_def_yr %>% apply(., 2, mean) 
gauges_df = gauges
boxplot( tot_deficit ~gauges$hydrogeo_simple)
class(tot_deficit)
y_val =mmky_ms30_min$sen_slope
ggplot()+
  geom_boxplot( aes(y=y_val, x=gauges_df$sr_new, colour=gauges_df$bfi_class), position = "dodge")+
  xlab("Hydrogeology")+
  ylab("cumulative discharge deficit during drought [m³]")+
  scale_color_discrete("BFI",labels = c("<0.4", "0.4-0.6", "0.6-0.8", ">0.8"))

plot(gauges$sr_new ~ gauges$bfi)



ggplot()+
  geom_boxplot( aes(y=tot_deficit, x=gauges$hydrogeo_simple, col=gauges$bfi_class), position = "dodge")+
  xlab("Hydrogeology")+
  ylab("cumulative discharge deficit during drought [m³]")+
  scale_color_discrete("BFI",labels = c("<0.4", "0.4-0.6", "0.6-0.8", ">0.8"))

ggsave("./plots/clustering/hydrogeo_deficit.pdf")

gauges$hydrogeo_simple

ggplot()+
  geom_boxplot( aes(y=mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05], x=gauges$hydrogeo_simple[mmky_ms30_min$new_p < 0.05], col=gauges$bfi_class[mmky_ms30_min$new_p < 0.05]), position = "dodge")+
  xlab("Hydrogeology")+
  ylab("ms30 trend (slope) [m³/s/a]")+
  # annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(mmky_ms7_min$new_p < 0.05))))+
  # annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label="p = 0.05")+
  scale_color_discrete("BFI",labels = c("<0.4", "0.4-0.6", "0.6-0.8", ">0.8"))

ggsave("./plots/clustering/hydrogeo_ms30_bfi.pdf")
fs_ms7_date
mmky_su
give.n <- function(x){
  return(c(y = median(x)+2.5, label = length(x)))
  # experiment with the multiplier to find the perfect position
}
temp_df = cbind.data.frame(as.numeric(mmky_ms7_date$sen_slope[mmky_ms7_date$new_p < fs_ms7_date]), gauges$hydrogeo_simple[mmky_ms7_date$new_p < fs_ms7_date],gauges$bfi_class[mmky_ms7_date$new_p < fs_ms7_date],gauges$sr_new[mmky_ms7_date$new_p < fs_ms7_date])
colnames(temp_df) = c("date","hydrogeo","bfi", "sr")

ggplot(temp_df, aes(x= factor(hydrogeo),y= date))+
  geom_boxplot() +
     stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  xlab("Hydrogeology")+
  ylab("ms7 timing trend (slope) [d/y]")+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(mmky_ms7_date$new_p < fs_ms7_date))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label="p = 0.03")
  
ggsave("./plots/clustering/hydrogeo_ms7_timing.pdf")

ggplot(temp_df, aes(x= factor(hydrogeo),y= date, color=factor(sr)))+
  geom_boxplot() +
     stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  xlab("Hydrogeology")+
  ylab("ms7 timing trend (slope) [d/y]")+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(mmky_ms7_date$new_p < 0.05))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label="p = 0.05")
  
ggsave("./plots/clustering/hydrogeo_ms7_timing2.pdf")

temp_df = cbind.data.frame(sr =gauges_df$sr_new[mmky_ms30_min$new_p < fs_ms30], ms30= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < fs_ms30])


ggplot(data = temp_df, aes(x= factor(sr),y= ms30))+
  geom_boxplot() +
     stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  xlab("Seasonality")+
  scale_x_discrete(labels=c("Summer","Winter"))+
  ylab("ms30 trend (slope) [m³/s/y]")+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(mmky_ms30_min$new_p < fs_ms30))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label="p = 0.07")
  
ggsave("./plots/clustering/sr.pdf")

#comparison between ssi drought definition and 80th method drought definition####
#ssi method: discharge below ssi > -1 is equal to drought. intensity, severity and drought length rely on deviation of the SSI value of that month. Leading to droughts that always last at least one month
#80th method: 30 day moving average deviation of the 20th quantile = drought

plot(gauges$n_events ~ gauges$bfi)


#clustered plots #### 
#see barker et al

 gauges_df = as.data.frame(gauges)



ggplot(data = gauges_df)+
  geom_point(aes(x=lt_memoryeffect, y= bfi))

ggsave("./plots/clustering/bfi_memoryeffect.pdf")
 
 ggplot(data=gauges_df)+
   geom_point(aes(x=, y= bfi, col= lt_memoryeffect))+
   xlab("mean drought length [d]")+
   ylab("BFI")+
   scale_color_continuous("Aquifer \nmemory")
ggsave("./plots/clustering/mn_length_bfi_lt.png")

ggplot()+
  geom_boxplot(aes(x=factor(gauges$sr_new[mmky_ms30_min$new_p < 0.05]), y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05]))

 t.test(x= mmky_ms7_min$sen_slope[mmky_ms7_min$new_p < 0.05 & gauges$sr_new == 0], y = mmky_ms7_min$sen_slope[mmky_ms7_min$new_p < 0.05 & gauges$sr_new == 2])
 which(mmky_ms7_date$sen_slope >0 & mmky_ms7_date$new_p < 0.05) %>% length()
 
 
ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= best_spei, col= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= cor_spi, col= as.factor(sr_new)))
ggsave("best_spi_cor_spi_saar.png")

ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= cor_spi, col= bfi))
ggsave("bfi_cor_spei_saar.png")
ggplot(data=gauges_df)+
  geom_point(aes(x=Enzgsg_, y= med_dr_sev, col= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=max_dr_sev, y= n_events, col= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=n_events, y= max_dr_dur, col = bfi))
ggsave("max_dr_dur_med_dr_int_bfi.png")

lm(gauges$cor_spi_dr ~ log10((gauges$saar*-1)^2)) %>% summary
hist((gauges$saar*-1)^2 %>% log10)
hist(gauges$cor_spi_dr )

plot(y=gauges$cor_spi_dr ,x= (gauges$saar*-1)^2)
lines(fm)

gauges$ms7_date = mmky_ms7_date$sen_slope
min(mmky_ms7_date$sen_slope)
rtb = c('#ca0020','#f4a582','#bababa','#92c5de','#0571b0') # colors for spplot


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

gauges$ms30 = mmky_ms30_min$sen_slope
gauges$ms30[mmky_ms30_min$new_p < fs_ms30] = 5
gauges$ms30 = cut(gauges$ms30, breaks = c(-3,-0.001,0.001,3,5)) 
tofy2 = c("#de2d26","#9ecae1","#3182bd","#bdbdbd")

pdf(file= "./plots/clustering/ms30_trends.pdf")
spplot(gauges, "ms30", 
      col.regions = tofy2,
       
       legendEntries = c("negative", "~0",  "positive","not significant"),
       sp.layout = germany,
     #  colorkey=T#,
      scales = list(draw = TRUE)
      )
dev.off()

spplot(gauges, "cor_spi_n")
gauges$cor_spi_n

boxplot(gauges_df$bfi ~ gauges_df$cor_spi_n_dr, horizontal=T )

plot(gauges$cor_spi_dr ~ gauges$saar)
