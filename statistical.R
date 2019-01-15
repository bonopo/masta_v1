#statistical correctness####

#significant correlation####
n = length(spi_v2_1$`1`)
cor = cor_sci_ssi(sci="spi_v2_")
t = cor*sqrt((n-2)/(1-(cor^2)))
df=n-2
qt(0.05/2, df=df, lower.tail = T)

#if t statistic is above or below the critical t vlaues than the correlation is significant


#significant trend####


y= mar_mn_q[,203]
fm = lm(y~c(1:40)) 
n = length(y)
trend =  mmky_mar_mn_q$tau[203]

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


bbsmK(x=y)
bbsmK_mod(x=y)
