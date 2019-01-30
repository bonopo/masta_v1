#statistical correctness####

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


res= field.significance(loc_sig = 0.1, data_x= ms30_min, global_sig=0.1, nsim=600)

hist(res)
quantile(res,.05)
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
#anova if alpine catchments have a significant lower yearly temperature increase than non-alpine catchments#
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

install.packages("zyp")
library(zyp)
mmky_mar_mn_q %>% head()
zyp::zyp.trend.vector(mt_mn_q_wide$`1`, method="yuepilon")

install.packages("EnvStats")
library(EnvStats)

kendallTrendTest(y= mar_mn_q$`1` )
mmky_mar_mn_q[1,]
(1.96 * sd(mmky_mar_mn_q$S))/2 +
  
qt(1-(.95/2), 38) * sqrt(7366)
relaimpo::calc.relimp()


#t.test####
t.test(x = mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 0])

hist(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2] %>% exp())

wilcox.test(x = mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < 0.05 & gauges$sr_new == 0])

wilcox.test(x = mmky_ms7_date$sen_slope[mmky_ms7_date$new_p < 0.05 & gauges$sr_new == 2], y= mmky_ms7_date$sen_slope[mmky_ms7_date$new_p < 0.05 & gauges$sr_new == 0])
