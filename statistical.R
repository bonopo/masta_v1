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

fs_wi_mn_q= field.significance(loc_sig = 0.05, data_x= wi_mn_q, global_sig=0.05, nsim=600)

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

save(list=c("fs_ms30","fs_ms7_date","fs_ms7","fs_wd_0","fs_yrd_0" ,  "fs_yr_mn_t","fs_su_mn_t", "fs_wi_mn_t","fs_jun_mn_q","fs_mar_mn_q","fs_sp_mn_t","fs_min_t","fs_max_t","fs_spd_0","fs_wi_sm_p", "fs_mt_p_pet","fs_mt_q","fs_yr_sm_p","fs_yr_sm_p_no70","fs_wi_mn_q","fs_q10","fs_mt_pet","fs_sm_p","fs_mn_t","fs_mt_med_q"), file="./output/fs.Rdata")
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
#significant
hist(residuals(fm))
hist(mmky_wi_mn_t$sen_slope)
gauges$mn_t %>% hist()

data_plot = cbind.data.frame(y= mmky_wi_mn_t$sen_slope, x1=gauges$mn_t, x2= gauges$Hochwrt)

ggplot(data= data_plot, aes(y=y, x=x1, col=x2))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
  annotate( geom="text", -Inf, Inf,  hjust = -0.2, vjust = 2.5, label=paste("n = ", length(data_plot$y)))+
  annotate(geom="text", -Inf, Inf,  hjust = -0.2, vjust = 1, label=paste("p = 0.05"))+
  annotate(geom="text", -Inf, Inf,  hjust =-0.2, vjust = 4, label=paste("r²=",round(fm$adj.r.squared,2)))+
  xlab("mean t [°C]")+
  ylab("winter mean t trend (slope) [°C/a]")+
  scale_color_continuous("Hochwert")

ggsave("./plots/statistical/winter_mn_t.pdf")

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
wilcox.test.modified(x_m = "mmky_su_mn_t")#just significant
wilcox.test.modified(x_m = "mmky_wi_sm_p")#just significant
boxplot( mmky_wi_mn_t$sen_slope[mmky_wi_mn_t$new_p < fs_wi_mn_t] ~ factor(gauges$sr_new))
length(which(mmky_su_mn_t$sen_slope < fs_su_mn_t))


t.test(x = mmky_wi_sm_p$sen_slope[mmky_wi_sm_p$new_p < fs_wi_sm_p & gauges$sr_new == 2], y= mmky_wi_sm_p$sen_slope[mmky_wi_sm_p$new_p < fs_wi_sm_p & gauges$sr_new == 0])

#76' drought effect ####

data_plot = yearly_sm_p %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sm_p,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

ggplot(data_plot %>% filter(gauge==1)  )+
         geom_smooth(aes(x=year, y=sm_p), method = "loess", span=0.3)+
  ylab("precipitation sum [mm/year]")+
    scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  xlab("")
  ggsave("./plots/statistical/yr_sm_p_loess.png")


ggplot()+
  geom_boxplot(data = data_plot,aes(x=as.factor(year), y=sm_p))+
  scale_x_discrete(labels=seq(1970,2010,by=5), breaks=seq(1970,2010,by=5))+
  xlab("")+
  ylab("precipitation sum [mm/year]")
   ggsave("./plots/statistical/box_yr_sm_p.pdf")
years= 1970:2009
res=NULL
for(i in 1:catch_n){
 res[i]=  years[which.min(yearly_sm_p[,i])]
}

table(res)

#temp trends
data_plot = yearly_mn_t %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sm_p,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

ggplot(data_plot)+
         geom_smooth(aes(x=year, y=sm_p), method = "loess", span=0.3)+
  ylab("temperature [°C/year]")+
    scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  xlab("")
  ggsave("./plots/statistical/yr_mn_t_loess.png")
  
#drought trends
  #q_days_of_drought_yr
  
   data_plot = q_days_of_drought_yr %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=days_q, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            mutate(sr=rep(gauges$sr_new, times=40) )%>% 
            group_by(year,sr) %>% 
            summarise(yr_med =  rollmedian(days_q, algin="center", k=catch_n), sd_neg=quantile(days_q,.25), sd_pos=quantile(days_q,.75)) %>% 
            as.tbl %>% 
            ungroup
  
  
   

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med), col="#56B4E9")+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col="#56B4E9")+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col="#56B4E9")+
  ylab("days of drought  [d/a]")+
  xlab("")
ggsave("./plots/statistical/drought_sum_q_winter.pdf")


loess()
  
  
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
  
  data_plot = q_sum_def_yr %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=sum_q, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            mutate(sr=rep(gauges$sr_new, times=40) )%>% 
            group_by(year,sr) %>% 
            summarise(yr_med = median(sum_q), sd_neg=quantile(sum_q,.25), sd_pos=quantile(sum_q,.75)) %>% 
            as.tbl
   

ggplot(data_plot %>% filter(sr==2))+
  geom_line(aes(x=year, y=yr_med), col="#56B4E9")+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col="#56B4E9")+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col="#56B4E9")+
  ylab("cumulative discharge deficit  [m³/a]")+
  xlab("")
ggsave("./plots/statistical/drought_sum_q_winter.pdf")

#p_sum_def




   data_plot = p_sum_def_yr %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=sum_p, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            mutate(sr=rep(gauges$sr_new, times=40) )%>% 
            group_by(year,sr) %>% 
            summarise(yr_med = median(sum_p), sd_neg=quantile(sum_p,.25), sd_pos=quantile(sum_p,.75)) %>% 
            as.tbl
   
   
   ggplot(data_plot %>% filter(sr==2))+
  geom_line(aes(x=year, y=yr_med), col=2)+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col=2)+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col=2)+
  ylab("cumulative precipitation deficit  [mm/a]")+
  xlab("")
ggsave("./plots/statistical/drought_sum_p_winter.pdf")



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
