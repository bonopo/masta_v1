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

s= mmky_su_p_pet$S
s_r = mean(s)
var(s_r)
#see barker ( i think)


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
