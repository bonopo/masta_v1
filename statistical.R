#statistical correctness####
#regime minima vs summer minima ####

cor(x= yearly_30_min[,gauges$sr_new == 0], y=ms30_min[,gauges$sr_new == 0]) %>% diag() %>% range
mmky_yearly_30_min$sen_slope[mmky_yearly_30_min$new_p< 0.05] %>% range
mmky_ms30_min$sen_slope[mmky_ms30_min$new_p< fs_ms30] %>% range
#significant correlation####
n = length(spi_v2_1$`1`)
cor = cor_sci_ssi(sci="spi_v2_")
t = cor*sqrt((n-2)/(1-(cor^2)))
df=n-2
qt(0.05/2, df=df, lower.tail = T)


z_score= 
#if t statistic is above or below the critical t vlaues than the correlation is significant


#significant trend####


y= mar_mn_q[,203]
fm = lm(y~c(1:40)) 
n = length(y)
trend =  mmky_mar_mn_q$tau[203]

z = trend - mean(mmky_mar_mn_q$corrected_z)/sqrt(mmky_mar_mn_q$new_var[203])

mmky_mar_mn_q$new_p[203]
2*pnorm(-abs(z))
mmky
t = trend*sqrt((n-2)/(1-(trend^2))) # t-test for significance
df=n-2
qt(0.05/2, df=df, lower.tail = F)
t

N_eff = n* ((1-acf(y, plot=F)$acf[2,1,1])/(1+acf(y, plot=F)$acf[2,1,1]))
variance = (1/(N_eff - 2)) * sum(residuals(fm)^2)
se= sqrt(variance/(sum(y-mean(y))^2))
t=  trend/se
mmky_mar_mn_q$sen_slope[120]

plot(mmky_mar_mn_q$corrected_z/sqrt(mmky_mar_mn_q$new_var), type="l")
lines(mmky_mar_mn_q$new_p, type="l", col=3)
legend("topright", c("corrected_z", "orig_z"), lty=c(1,1), col=c(1,3))

plot(yearly_sm_p$`47`, type="l")
abline(a= median(yearly_sm_p$`47`), b = mmky_yearly_sm_p$sen_slope[47])
which.min(mmky_su_sm_p$sen_slope)
40*mmky_su_sm_p$sen_slope[100]

#field significance ####
#see renard 2008 and Burn et al 2002
fs_su_sm_p= field.significance(loc_sig = 0.05, data_x= su_sm_p, global_sig=0.05, nsim=600)
fs_wi_sm_p = field.significance(loc_sig = 0.05, data_x= wi_sm_p, global_sig=0.05, nsim=600)
fs_q10 = field.significance(loc_sig = 0.05, data_x= wi_q10, global_sig=0.05, nsim=600)
fw_su_p_pet = field.significance(loc_sig = 0.05, data_x= su_p_pet, global_sig=0.05, nsim=600)
fs_wi_sm_p=  field.significance(loc_sig = 0.05, data_x= wi_sm_p, global_sig=0.05, nsim=600)
fs_mar_mn_q=  field.significance(loc_sig = 0.05, data_x= mar_mn_q, global_sig=0.05, nsim=600)
fs_jun_mn_q=  field.significance(loc_sig = 0.05, data_x= jun_mn_q, global_sig=0.05, nsim=600)
fs_wi_mn_t=  field.significance(loc_sig = 0.05, data_x= wi_mn_t, global_sig=0.05, nsim=600)
fs_su_mn_t=  field.significance(loc_sig = 0.05, data_x= yearly_mn_t, global_sig=0.05, nsim=600)
fs_yr_mn_t=  field.significance(loc_sig = 0.05, data_x= yearly_mn_t, global_sig=0.05, nsim=600)
fs_ms30= field.significance(loc_sig = 0.05, data_x= ms30_min, global_sig=0.05, nsim=600)
fs_ms7_date = field.significance(loc_sig = 0.05, data_x= ms7_date, global_sig=0.05, nsim=600)
fs_ms7 = field.significance(loc_sig = 0.05, data_x= ms7_min, global_sig=0.05, nsim=600)
fs_wd_0= field.significance(loc_sig = 0.05, data_x= wi_days_below_0, global_sig=0.05, nsim=600)
fs_yrd_0 = field.significance(loc_sig = 0.05, data_x= yr_days_below_0, global_sig=0.05, nsim=600)
fs_max_t = field.significance(loc_sig = 0.05, data_x= yearly_max_t, global_sig=0.05, nsim=600)
fs_min_t = field.significance(loc_sig = 0.05, data_x= yearly_min_t, global_sig=0.05, nsim=600)
fs_sp_mn_t = field.significance(loc_sig = 0.05, data_x= sp_mn_t, global_sig=0.05, nsim=600)
fs_sp_mn_t = field.significance(loc_sig = 0.05, data_x= sp_mn_t, global_sig=0.05, nsim=600)
fs_spd_0 = field.significance(loc_sig = 0.05, data_x= sp_days_below_0, global_sig=0.05, nsim=600)
fs_yr_sm_p = field.significance(loc_sig = 0.05, data_x= yearly_sm_p, global_sig=0.05, nsim=600)
fs_yr_sm_p_no70= field.significance(loc_sig = 0.05, data_x= yr_sm_p_no70, global_sig=0.05, nsim=600)
fs_dd_p= field.significance(loc_sig = 0.05, data_x= p_days_of_drought_yr, global_sig=0.05, nsim=600)
fs_dd_q = field.significance(loc_sig = 0.05, data_x= q_days_of_drought_yr, global_sig=0.05, nsim=600)
fs_wi_mn_q= field.significance(loc_sig = 0.05, data_x= wi_mn_q, global_sig=0.05, nsim=600)
fs_df_p= field.significance(loc_sig = 0.05, data_x= p_n_events_yr, global_sig=0.05, nsim=600)
fs_df_q= field.significance(loc_sig = 0.05, data_x= q_n_events_yr, global_sig=0.05, nsim=600)
fs_ds_p = field.significance(loc_sig = 0.05, data_x= p_sum_def_yr, global_sig=0.05, nsim=600)
fs_ds_q = field.significance(loc_sig = 0.05, data_x= q_sum_def_yr, global_sig=0.05, nsim=600)

fs_mt_p_pet= c()
for ( i in 1:12) {
fs_mt_p_pet[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_p_pet")), global_sig=0.05, nsim=600)
}
fs_mt_q= c()
for ( i in 1:12) {
fs_mt_q[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_mn_q")), global_sig=0.05, nsim=600)
}

fs_mt_med_q= c()
for ( i in 1:12) {
fs_mt_med_q[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_med_q")), global_sig=0.05, nsim=600)
}

fs_sm_p= c()
for ( i in 1:12) {
fs_sm_p[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_sm_p")), global_sig=0.05, nsim=600)
}
fs_mt_pet= c()
for ( i in 1:12) {
fs_mt_pet[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_pet")), global_sig=0.05, nsim=600)
}

fs_mn_t= c()
for ( i in 1:12) {
fs_mn_t[i] = field.significance(loc_sig = 0.05, data_x= get(paste0(str_to_lower(month.abb[i]),"_mn_t")), global_sig=0.05, nsim=600)
}

save(list=c("fs_ms30","fs_ms7_date","fs_ms7","fs_wd_0","fs_yrd_0" ,  "fs_yr_mn_t","fs_su_mn_t", "fs_wi_mn_t","fs_jun_mn_q","fs_mar_mn_q","fs_sp_mn_t","fs_min_t","fs_max_t","fs_spd_0","fs_wi_sm_p", "fs_mt_p_pet","fs_mt_q","fs_yr_sm_p","fs_yr_sm_p_no70","fs_wi_mn_q","fs_q10","fs_mt_pet","fs_sm_p","fs_mn_t","fs_mt_med_q", "fs_su_sm_p","fs_dd_p","fs_dd_q","fs_df_p","fs_df_q","fs_ds_p","fs_ds_q"), file="./output/fs.Rdata")
# linear regression -------------------------------------------------------

#ms30 ~ long term memory effect




data_plot = cbind.data.frame(y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05],x1= gauges$lt_memoryeffect[mmky_ms30_min$new_p < 0.05], x2=gauges$mn_deficit[mmky_ms30_min$new_p < 0.05])

fm= lm(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05] ~ gauges$lt_memoryeffect[mmky_ms30_min$new_p < 0.05]) %>% summary()

hist(data_plot$x2)
residuals(fm) %>% hist()

ggplot(data= data_plot, aes(y=y, x=x1, col=x2))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
  annotate( geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -2.5, label=paste("n = ", length(data_plot$y)))+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -1, label=paste("p = 0.05"))+
  annotate(geom="text", -Inf, -Inf,  hjust =-0.2, vjust = -4, label=paste("r²=",round(fm$adj.r.squared,2)))+
  xlab(paste("lt memory effect [cor. ssi ~ spi-12]"))+
  ylab(paste("ms30 trend (slope)"))+
  scale_color_continuous("mean drought \n deficit ln [m³]")

ggsave("./plots/statistical/ms30_ltmemory.png")

#climatic trends####
#anova####
#if alpine catchments have a significant lower yearly temperature increase than non-alpine catchments#
t.test(mmky_wi_mn_t$sen_slope, mmky_wi_mn_t$sen_slope, var.equal = F)

lm(mmky_su_mn_t$sen_slope ~ gauges$sr_new) %>% summary()
anova(lm(mmky_su_mn_t$sen_slope ~ gauges$sr_new)) %>% summary()



hist(mmky_su_mn_t$sen_slope[gauges$sr_new ==2] %>% sqrt()) #not normal so wilcoxen-mann-whitney test

wilcox.test(x= mmky_su_mn_t$sen_slope[gauges$sr_new ==2], mmky_su_mn_t$sen_slope[gauges$sr_new ==0], paired = F)


# significant? looking at graph: NO!!!!

fm = lm(mmky_wi_mn_t$sen_slope ~ gauges$mn_t) %>% summary() #significant
fm2 = lm(mmky_wi_mn_t$sen_slope[gauges$sr_new ==0] ~ gauges$Hochwrt[gauges$sr_new ==0]) %>% summary()
fm3 = lm(mmky_wi_mn_t$sen_slope ~ gauges$Hochwrt) %>% summary()
#significant
hist(residuals(fm2))
hist(mmky_wi_mn_t$sen_slope)
hist(gauges$Hochwrt[gauges$sr_new ==0])
gauges$mn_t %>% hist()

data_plot = cbind.data.frame(y= mmky_wi_mn_t$sen_slope[mmky_wi_mn_t$new_p < fs_wi_mn_t], x1=gauges$mn_t[mmky_wi_mn_t$new_p < fs_wi_mn_t], x2= gauges$Hochwrt[mmky_wi_mn_t$new_p < fs_wi_mn_t])


ggplot(data= data_plot, aes(y=y, x=x1, col=x2))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
  annotate( geom="text", -Inf, Inf,  hjust = -0.2, vjust = 2.5, label=paste("n = ", length(data_plot$y)))+
  annotate(geom="text", -Inf, Inf,  hjust = -0.2, vjust = 1, label=paste("p = 0.05"))+
  annotate(geom="text", -Inf, Inf,  hjust =-0.2, vjust = 4, label=paste("r²=",round(fm$adj.r.squared,2)))+
  xlab("")+
  ylab("wi. temp. trend (slope) [°C/a]")+
  scale_color_continuous("Hochwert")
ggsave("./plots/statistical/winter_mn_t2.pdf")

ggplot(data= data_plot, aes(y=y, x=x2))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
  annotate( geom="text", -Inf, Inf,  hjust = -0.2, vjust = 2.5, label=paste("n = ", length(data_plot$y)))+
  annotate(geom="text", -Inf, Inf,  hjust = -0.2, vjust = 1, label=paste("p =",round(fs_wi_mn_t,2)))+
  annotate(geom="text", -Inf, Inf,  hjust =-0.2, vjust = 4, label=paste("r²=",round(fm2$adj.r.squared,2)))+
  xlab("Latitude")+
  ylab("wi. temp. trend (slope) [°C/a]")

ggsave("./plots/statistical/winter_mn_t2.pdf")

aov(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < fs_ms30] ~ gauges$sr_new[mmky_ms30_min$new_p < fs_ms30]) %>% summary()
hist(gauges$hydrogeo_simple[mmky_ms7_date$new_p < 0.05])

kruskal.test(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < fs_ms30] ~ gauges$sr_new[mmky_ms30_min$new_p < fs_ms30])

hist(mmky_ms7_date$sen_slope[mmky_ms30_min$new_p < fs_ms30&gauges$sr_new == 2])

 mmky_ms7_date$sen_slope[mmky_ms7_date$new_p < fs_ms7_date&gauges$sr_new == 2] %>% sqrt %>% hist
## decompose time series into trend and seasonal part ---------------------

install.packages("fpp")
require(fpp)
ts = ts(mt_mn_q_wide, start=c(1970,1), end=c(2009,12), deltat=1/12)
ssi_dec <- decompose(ts[,1])
plot(ssi_dec)
res=decompose(ts)
plot(res)
res$trend[,1] %>% plot()
stl_res = stl(ts[,2], "periodic")
str(stl_res)
plot(stl_res)
trend = stl_res$time.series[,2]
seas = stl_res$time.series[,1]
dum_var = cbind(trend, seas)
y= ts[,2]
fit2 = tslm(y ~ trend + season)
n <- length(y)
plot(y)
lines(ts(fit2$coef[1]+fit2$coef[2]*(1:n)+mean(fit2$coef[-(1:2)]),
  start=start(y),f=12),col="red")

trend = fit2$coef[2] ##this is the linear trend
#compare to mmky sen's slope
mmky(mt_mn_q_wide$`1`)
res = arima(y, xreg =dum_var) %>% summary()

str(res)

#sen's slope conf intervals####
error.bar(data = "mar_mn_q")
error.bar("jun_mn_q")

#t.test####
t.test(x = mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 0])

hist(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2] %>% exp())

wilcox.test(x = mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 0])

wilcox.test(x = mmky_ms7_date$sen_slope[mmky_ms7_date$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms7_date$sen_slope[mmky_ms7_date$new_p < 0.05 & gauges$sr_new == 0])

wilcox.test(x = mmky_yearly_mn_t$sen_slope[mmky_yearly_mn_t$new_p < fs_yr_mn_t & gauges$sr_new == 2], y= mmky_yearly_mn_t$sen_slope[mmky_yearly_mn_t$new_p < fs_yr_mn_t & gauges$sr_new == 0])

wilcox.test(x = mmky_yearly_mn_t$sen_slope[mmky_yearly_mn_t$new_p < fs_yr_mn_t & gauges$sr_new == 2], y= mmky_yearly_mn_t$sen_slope[mmky_yearly_mn_t$new_p < fs_yr_mn_t & gauges$sr_new == 0])

wilcox.test(x = mmky_wi_mn_t$sen_slope[mmky_wi_mn_t$new_p < fs_yr_mn_t & gauges$sr_new == 2], y= mmky_wi_mn_t$sen_slope[mmky_wi_mn_t$new_p < fs_yr_mn_t & gauges$sr_new == 0])
wilcox.test.modified(x_m = "mmky_wi_mn_t")
wilcox.test.modified(x_m = "mmky_ms7_min",fs_x =  fs_ms7 )
wilcox.test.modified(x_m = "mmky_su_mn_t")#just significant
wilcox.test.modified(x_m = "mmky_wi_sm_p")#just significant
boxplot( mmky_wi_mn_t$sen_slope[mmky_wi_mn_t$new_p < fs_wi_mn_t] ~ factor(gauges$sr_new))
length(which(mmky_su_mn_t$sen_slope < fs_su_mn_t))

t.test(x= gauges$cor_spi[gauges$sr_new == 2],y = gauges$cor_spi[gauges$sr_new == 0])

t.test(x = mmky_wi_sm_p$sen_slope[mmky_wi_sm_p$new_p < fs_wi_sm_p & gauges$sr_new == 2], y= mmky_wi_sm_p$sen_slope[mmky_wi_sm_p$new_p < fs_wi_sm_p & gauges$sr_new == 0])

#time series plots ####
#precipitation####
data_plot = yearly_sm_p %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sm_p,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

ggplot()+
  geom_boxplot(data = data_plot,aes(x=as.factor(year), y=sm_p))+
  scale_x_discrete(labels=seq(1970,2010,by=5), breaks=seq(1970,2010,by=5))+
  xlab("")+
  ylab("precipitation sum [mm/year]")
   ggsave("./plots/statistical/box_yr_sm_p.pdf")

    mn_yr = rollapply(data= yearly_sm_p, FUN= mean,  width=1, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= yearly_sm_p, FUN= quantile,probs = c(0.25, .75),  width=1, align="center",fill = NA, by.column = F)
 data_plot = cbind.data.frame(1970:2009, mn_yr, sd) %>% set_colnames(c("year","yr_mn","sd_neg","sd_pos"))
mean(data_plot$yr_mn)
major_drought = data_plot$year[which(data_plot$yr_mn < quantile(data_plot$yr_mn,.2))] #defined as 20th quantile of the yearly precipitation sum
 
 ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_mn), col=1)+
geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey", alpha=.4)+
  ylab("yearly mean temperature [°C]")+
  xlab("")+
  theme_bw()
 
 ggsave("./plots/statistical/yearly_mm.pdf")

 
# ggplot(data_plot %>% filter(gauge==1)  )+
#          geom_smooth(aes(x=year, y=sm_p), method = "loess", span=0.3)+
#   ylab("precipitation sum [mm/year]")+
#     scale_color_discrete("Seasonality", labels=c("summer","winter"))+
#   xlab("")
#   ggsave("./plots/statistical/yr_sm_p_loess.png")


#major drought events (according to precipitation deficit)
years= 1970:2009
res=NULL
for(i in 1:catch_n){
 res[i]=  years[which.min(yearly_sm_p[,i])]
}

table(res)

#temp ####
 med_yr = rollapply(data= yearly_mn_t, FUN= mean,  width=3, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= yearly_mn_t, FUN= quantile,probs = c(0.25, .75),  width=3, align="center",fill = NA, by.column = F)
 data_plot = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med), col=1)+
geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey", alpha=.4)+
  ylab("yearly mean temperature [°C]")+
  xlab("")+
  theme_bw()
ggsave("./plots/statistical/yr_temp_smooth.png")

#temp monthly
#april(as it has very high trend)
med_yr = rollapply(data= apr_mn_t, FUN= mean,  width=3, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= apr_mn_t, FUN= quantile,probs = c(0.25, .75),  width=3, align="center",fill = NA, by.column = F)
 data_plot = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med), col=1)+
geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey", alpha=.4)+
  ylab("April mean temperature [°C]")+
  xlab("")+
  theme_bw()
ggsave("./plots/statistical/april_temp.pdf")


#may
med_yr = rollapply(data= may_mn_t, FUN= mean,  width=3, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= may_mn_t, FUN= quantile,probs = c(0.25, .75),  width=3, align="center",fill = NA, by.column = F)
 data_plot = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med), col=1)+
geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey", alpha=.4)+
  ylab("May mean temperature [°C]")+
  xlab("")+
  theme_bw()
ggsave("./plots/statistical/may_temp.pdf")

# ggplot(data_plot)+
#          geom_smooth(aes(x=year, y=yr_med), method = "loess", span=0.3)+
#   ylab("temperature [°C/year]")+
#     scale_color_discrete("Seasonality", labels=c("summer","winter"))+
#   xlab("")
#   ggsave("./plots/statistical/yr_mn_t_loess.png")
  
  #drought freq####

# med_yr = rollapply(data= q_drought_freq, FUN= sum,  width=5, by=2,  align="center", by.column = T) 

mn_dr_freq = apply(q_drought_freq,1,mean) #med_yr
sd_dr_freq= apply(q_drought_freq,1,quantile, probs=c(.25,.75)) %>% t
data_plot = cbind.data.frame(seq(1970,2009, 1), mn_dr_freq, sd_dr_freq) %>% set_colnames(c("year","yr_mn","sd_neg","sd_pos"))#seq(1974,2009, 5)1970:2009
 
 ggplot(data_plot)+
 geom_line(aes(x=year, y=yr_mn, color = "mean n droughts  "), linetype = "solid", lwd=1.3)+ #linetype nur als beispiel geändert!!!
    geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey50", alpha=.3)+ #alternativ kann man auch geom_polygone nutzen
    geom_blank(aes(color = "IQR "))+ #bracht man für den legendentrick
    labs(y = "mean number of drougths [n/a/catchment]",
         x = element_blank())+
   scale_color_manual(element_blank(), values = c("mean n droughts  " = "black",  "IQR " = "grey50"), guide = guide_legend(override.aes = list(size = c(3.5,1.3), linetype = c("solid", "solid"), alpha = c(.3,1)
    )))+
    nice
 
 ggsave("./plots/statistical/drought_freq.pdf")
 
 
#seasonal drought freq#
 mn_dr_freq = apply(summer_p_drought_freq,1,mean) #med_yr
sd_dr_freq= apply(summer_p_drought_freq,1,quantile, probs=c(.25,.75)) %>% t
data_plot = cbind.data.frame(seq(1970,2009, 1), mn_dr_freq, sd_dr_freq) %>% set_colnames(c("year","yr_mn","sd_neg","sd_pos"))#seq(1974,2009, 5)1970:2009
 
 ggplot(data_plot)+
 geom_line(aes(x=year, y=yr_mn, color = "mean n droughts  "), linetype = "solid", lwd=1.3)+ #linetype nur als beispiel geändert!!!
    geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey50", alpha=.3)+ #alternativ kann man auch geom_polygone nutzen
    geom_blank(aes(color = "IQR "))+ #bracht man für den legendentrick
    labs(y = "mean number of drougths [n/a/catchment]",
         x = element_blank())+
   scale_color_manual(element_blank(), values = c("mean n droughts  " = "black",  "IQR " = "grey50"), guide = guide_legend(override.aes = list(size = c(3.5,1.3), linetype = c("solid", "solid"), alpha = c(.3,1)
    )))+
    nice
 

  #temp + precip + streamflow####
  
  mn_t = rollapply(data= spei_v2_3, FUN= mean, na.rm=T, width=30, align="center",fill = NA, by.column = F)
      mn_p = rollapply(data= spi_v2_3, FUN= mean, na.rm=T, width=30, align="center",fill = NA, by.column = F)
     mn_q= rollapply(data= ssi_1[,1:catch_n], FUN= mean, na.rm=T, width=30, align="center",fill = NA, by.column = F)
  
 
     
ggplot()+
    theme_bw()+
    geom_line(aes(x=date_seq, y=mn_t, color="SPEI-3"), lwd=1, alpha=1)+
   geom_line(aes(x=date_seq, y=mn_p, color= "SPI-3"), lwd=1, alpha=1)+
   geom_line(aes(x=date_seq, y=mn_q, color="SSI"), lwd=1, alpha=.5)+
      xlab("")+
         ylab("SCI")+
      scale_color_discrete("")+
  theme(legend.position="bottom")+
  geom_hline(yintercept = 0)

#+
   scale_color_manual(element_blank(), values = c(  "SPI-3" = "orange","SPEI-3" = "green", "SSI" = "blue"), guide = guide_legend(override.aes = list(size = c(1,1,1), linetype = c("solid", "solid","solid"), alpha = c(1,1,.8)
    )))+
    nice+
  geom_hline(yintercept = 0)
  
  ggsave("./plots/statistical/sci_2.pdf")
        

   t.test(x = gauges$mn_deficit[gauges$hydrogeo_simple == "K"] ,y = gauges$mn_deficit[gauges$hydrogeo_simple == "P"])
  
  
  ggplot(data_plot)+
  geom_line(aes(x=year, y=mn_t), col=2)+
  geom_line(aes(x=year, y=mn_q), col=4)+
  geom_line(aes(x=year, y=mn_p), col=1)+
  scale_y_log10()
  ylab("")+
  xlab("")+
  theme_bw()
#drought trends####
  #q_days_of_drought_yr
  med_yr = rollapply(data= q_days_of_drought_yr, FUN= median,  width=1, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= q_days_of_drought_yr, FUN= quantile,probs = c(0.25, .75),  width=1, align="center",fill = NA, by.column = F)
 data_plot_q = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 
 apply(q_days_of_drought_yr,2,mean) %>% mean
  apply(p_days_of_drought_yr,2,mean) %>% mean
 
  apply(p_n_events_yr,2,mean) %>% mean
    apply(q_n_events_yr,2,mean) %>% mean
    
   apply(q_days_of_drought_yr,2,sd) %>% sd
  apply(p_days_of_drought_yr,2,sd) %>% sd
  
  med_yr = rollapply(data= p_days_of_drought_yr, FUN= median,  width=1, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= p_days_of_drought_yr, FUN= quantile,probs = c(0.25, .75),  width=1, align="center",fill = NA, by.column = F)
 data_plot_p = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 
 
 
ggplot(data_plot_q)+
  geom_line(aes(x=year, y=yr_med), col="#2b8cbe", lwd=1)+
  geom_ribbon(data = data_plot_q, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#56B4E9", alpha=.3)+
  ylab("days of drought  [d/a]")+
  xlab("")+
    geom_line(data=data_plot_p,aes(x=year, y=yr_med), col="#de2d26",show.legend = T, lwd=1 )+
    geom_ribbon(data = data_plot_p, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#fc9272", alpha=.3,show.legend = T )+
  theme_bw()
  ggsave("./plots/d.pdf", plot = p)
  
  
 
ggsave("./plots/statistical/days_of_drought.pdf")

#same plot but with legend

 

#theme
nice <- theme_bw()+
    theme(legend.position = "bottom",
          text = element_text(size = 12)) 
        

ggplot(data_plot_q)+
    geom_line(aes(x=year, y=yr_med, color = "streamflow  "), linetype = "solid", lwd=1.3)+ #linetype nur als beispiel geändert!!!
    geom_ribbon(data = data_plot_q, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#56B4E9", alpha=.3)+ #alternativ kann man auch geom_polygone nutzen
    geom_blank(aes(color = "IQR "))+ #bracht man für den legendentrick
    labs(y = "days of drought  [d/a]",
         x = element_blank())+ #besser so dann wird kein "platz" freigehalten, element blank geht nur mit dem labs befehl!!!
    geom_line(data=data_plot_p,aes(x=year, y=yr_med, color = "precipitation  "), lwd=1.3)+
    geom_ribbon(data = data_plot_p, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#fc9272", alpha=.3)+ #wichtig das legend=t muss raus!!!
    geom_blank(aes(color = "IQR  "))+
   scale_color_manual(element_blank(), values = c("streamflow  " = "#2b8cbe", "precipitation  " = "#de2d26", "IQR " = "#56B4E9", "IQR  " = "#fc9272"), guide = guide_legend(override.aes = list(size = c(3.5, 3.5,1.3,1.3), linetype = c("solid", "solid", "solid", "solid"), alpha = c(.3,.3,1,1)
    )))+
    nice
#scale_color_manuel: element blank steht für keine Überschrift über legende. elemente werden immer alphabetisch sortiert. zur not mit Leerzeichen schummeln. merkt man nicht!
    ggsave("./plots/statistical/days_drought_med.pdf")

 
  
p= ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med), col="#56B4E9")+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col="#56B4E9")+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col="#56B4E9")+
  ylab("days of drought  [d/a]")+
  xlab("")


grid.arrange(p,q, ncol=1)  


  data_plot = q_days_of_drought_yr %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=days_dr,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl
  data_plot = p_days_of_drought_yr %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=days_dr,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

  
  ggplot(data_plot,aes(x=year, y=days_dr))+
    geom_smooth( method = "loess", span=.3)+
    facet_wrap(~sr)
  
  ggplot(data_plot,aes (x=year, y=days_dr, col=as.factor(gauge)))+
    geom_smooth(show.legend = F, se=F, span=.3, method="loess")
  
 ggplot(data_plot )+
         geom_smooth(aes(x=year, y=days_dr,col="darkblue"), method = "loess", span=.3)#+
       # geom_smooth(data= data_plot2, aes(x=year, y=days_dr,col="red"), method = "loess", span=0.2)+
  ylab("days of drought [d/year]")+
  xlab("")+
  scale_color_discrete("Drought indice", labels=c("streamflow","precipitation"))
  ggsave("./plots/statistical/drought_days.png")
  
  #q_sum_def_yr
  q_sum_def_stan = (q_sum_def_yr-apply(q_sum_def_yr,2,mean))/apply(q_sum_def_yr,2,sd) #standartized
  
  data_plot_q = q_sum_def_stan %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=sum_q, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            #mutate(sr=rep(gauges$sr_new, times=40) )%>% 
            group_by(year) %>% 
            summarise(yr_med = median(sum_q), sd_neg=quantile(sum_q,.25), sd_pos=quantile(sum_q,.75)) %>% 
            as.tbl
   

# q= ggplot(data_plot %>% filter(sr==2))+
#   geom_line(aes(x=year, y=yr_med), col="#2b8cbe")+
#  geom_ribbon(aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#56B4E9", alpha=.3)+
#   ylab("cum. q deficit  [m³/a]")+
#   xlab("")
#ggsave("./plots/statistical/drought_sum_q_winter.pdf")

#p_sum_def


p_sum_def_stan = (p_sum_def_yr-apply(p_sum_def_yr,2,mean))/apply(p_sum_def_yr,2,sd) #standartized

   data_plot_p = p_sum_def_stan %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=sum_p, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            #mutate(sr=rep(gauges$sr_new, times=40) )%>% 
            group_by(year) %>% 
            summarise(yr_med = median(sum_p), sd_neg=quantile(sum_p,.25), sd_pos=quantile(sum_p,.75)) %>% 
            as.tbl
   
   
ggplot(data_plot_p)+
  geom_line(aes(x=year, y=yr_med), col="#de2d26")+
  geom_ribbon(aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#fc9272", alpha=.3)+
  geom_line(data = data_plot_q, aes(x=year, y=yr_med), col="#2b8cbe")+
  geom_ribbon(data = data_plot_q, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#56B4E9", alpha=.3)+
  ylab("standardized cumulative deficit")+
  xlab("")
#ggsave("./plots/statistical/drought_sum_p_winter.pdf")

ggplot(data_plot_q)+
    geom_line(aes(x=year, y=yr_med, color = "streamflow  "), linetype = "solid", lwd=1.3)+ #linetype nur als beispiel geändert!!!
    geom_ribbon(data = data_plot_q, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#56B4E9", alpha=.3)+ #alternativ kann man auch geom_polygone nutzen
    geom_blank(aes(color = "IQR "))+ #bracht man für den legendentrick
    labs(y = "mean standardized cum. deficit",
         x = element_blank())+ #besser so dann wird kein "platz" freigehalten, element blank geht nur mit dem labs befehl!!!
    geom_line(data=data_plot_p,aes(x=year, y=yr_med, color = "precipitation  "), lwd=1.3)+
    geom_ribbon(data = data_plot_p, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "#fc9272", alpha=.3)+ #wichtig das legend=t muss raus!!!
    geom_blank(aes(color = "IQR  "))+
   scale_color_manual(element_blank(), values = c("streamflow  " = "#2b8cbe", "precipitation  " = "#de2d26", "IQR " = "#56B4E9", "IQR  " = "#fc9272"), guide = guide_legend(override.aes = list(size = c(3.5, 3.5,1.3,1.3), linetype = c("solid", "solid", "solid", "solid"), alpha = c(.3,.3,1,1)
    )))+
    nice
ggsave("./plots/statistical/cum_def.pdf")


grid.arrange(p,q,ncol=1)

   data_plot = q_sum_def_yr %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sum_def,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl
  data_plot2 = p_sum_def_yr %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sum_def,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

ggplot(data_plot)+
         geom_smooth(aes(x=year, y=sum_def), method = "loess", span=0.3, col="darkblue")+
         ylab("sum deficit [m³/year]")+
         xlab("")
ggsave("./plots/statistical/q_sum_def.png")

ggplot()+
        geom_smooth(data= data_plot2, aes(x=year, y=sum_def), method = "loess", span=0.3,col="red")+
  ylab("sum deficit [mm/year]")+
  xlab("")

  ggsave("./plots/statistical/p_sum_def.png")
  
  #summer drought characteristics
  
  med_yr = rollapply(data= summer_dy_drought_q, FUN= mean,  width=1, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= summer_dy_drought_q, FUN= quantile,probs = c(0.25, .75),  width=1, align="center",fill = NA, by.column = F)
 data_plot_q = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 
   med_yr = rollapply(data= summer_dy_drought_p, FUN= mean,  width=1, align="center",fill = NA, by.column = F)
  sd=  rollapply(data= summer_dy_drought_p, FUN= quantile,probs = c(0.25, .75),  width=1, align="center",fill = NA, by.column = F)
 data_plot_p = cbind.data.frame(1970:2009, med_yr, sd) %>% set_colnames(c("year","yr_med","sd_neg","sd_pos"))
 

ggplot(data_plot_p)+
  geom_line(aes(x=year, y=yr_med), col=2)+
geom_ribbon(data = data_plot, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey", alpha=.4)+
    geom_line(data= data_plot_q, aes(x=year, y=yr_med), col=4)+
geom_ribbon(data= data_plot_q, aes(x= year, ymin = sd_neg, ymax= sd_pos), fill = "grey", alpha=.4)+
# ylab("yearly mean temperature [°C]")+
  xlab("")+
  theme_bw()
table(res)
