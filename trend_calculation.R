# Trend analysis ----------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
source("./R/masta_v1/functions.R")# has to run before if not objects will be missing!
source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missing!
source("./R/masta_v1/sci_calculation.R")# has to run before if not objects will be missin!
source("./R/masta_v1/climate_characteristics.R")
source("./R/masta_v1/drought_characteristics.R") # has to run before if not objects will be missin!
source("./R/masta_v1/clustering.R", echo = TRUE, print.eval=TRUE)


#acf####
res = matrix(nrow=catch_n, ncol=6)
c=1
for (d in c("yearly_mean_q" ,"yearly_min_q","summer_ave_q","summer_min_q","summer_q_q10","nq_monthly")){
data_temp = get(d)
for(r in 1:catch_n){
  temp = acf(data_temp[,r], plot=FALSE, type = "correlation")
  res[r,c] = temp$acf[2]
}
c=c+1
}
res = as.data.frame(res)
colnames(res) = c("yearly_mean_q" ,"yearly_min_q","summer_ave_q","summer_min_q","summer_q_q10","nq_monthly")
acf_df = res

for(i in 1:6){
png(filename = paste0(colnames(acf_df)[i],".png"))
plot(acf_df[,i], type="p", xlab=colnames(acf_df)[i], ylab="correlation at lag=1", ylim=c(-.2, 1))
abline(h=0, col=3, lty=2)
abline(h=c(1.96/sqrt(length(acf_df[,i])), -1.96/sqrt(length(acf_df[,i]))), lty=2, col=2)
legend("topleft", col=c(1,2), pch=c(1, NA), lty=c(NA, 2), c("ACF", "p value"), bty="n")
dev.off()
}

#mann kendall with AR correction ####
# mk_tests_par(raw_data = "jun_mean_df")
# mk_tests_par(raw_data = c("mar_mean_df","jul_mean_df"))

mmky_par( c("yr_days_below_0", "wi_days_below_0"))
mmky_par(c("sp_mn_t", "sp_sm_p"))
mmky_par(c("su_mn_q","wi_mn_q"))
mmky_par(c("wi_med_q", "su_med_q"))
mmky_par("q_drought_freq")
mmky(wi_days_below_0$`1`)
mmky_par(c("su_p_pet", "yearly_30_min", "yearly_7_min", "yearly_7_date","mw7_min", "mw7_date"))
class(wi_days_below_0)
any(is.infinite(wi_days_below_0))

mmky_par(raw_data = c( "ms7_date", "ms7_min", "ms30_min", "yearly_q10","yearly_mn_q","su_q10", "wi_q10", "su_mn_t", "wi_mn_t","yearly_mn_t", "yearly_max_t", "yearly_sm_p",    "su_sm_p", "wi_sm_p", "sp_sm_p", "year_p_pet", "su_p_pet", "wi_p_pet"))

mmky_par(raw_data = c( "p_days_of_drought_yr" ,"q_days_of_drought_yr","p_sum_def_yr","q_sum_def_yr", "p_n_events_yr", "q_n_events_yr"))

mmky_par(raw_data = c("march_dy_drought_q", "march_dy_drought_p","march_sm_def_p","march_sm_def_q","june_dy_drought_q", "june_dy_drought_p","june_sm_def_p","june_sm_def_q")) # no trends can be seen!



mmky_par(raw_data = c( "def_vol_q","def_vol_p","days_dr_q","days_dr_p"))


monthly_dy_drought=c()
for ( i in 1:12) monthly_dy_drought[i] =paste0(str_to_lower(month.abb[i]),"_dy_drought_q")
mmky_par(raw_data = monthly_dy_drought)

monthly_sum_def=c()
for ( i in 1:12) monthly_sum_def[i] =paste0(str_to_lower(month.abb[i]),"_sum_drought_q")
mmky_par(raw_data = monthly_sum_def)

monthly_p_pet=c()
for ( i in 1:12) monthly_p_pet[i] =paste0(str_to_lower(month.abb[i]),"_p_pet")
mmky_par(raw_data = monthly_p_pet)

png("./plots/further_investigate/final/sen_vs_p_value.png", width=1000, height=500)
par(mfrow=c(2,1))
plot(mmky_ms7_min$sen_slope ~ mmky_ms7_min$new_p, ylab="mmky sen's slope")
plot(mmkh_ms7_min$sen_slope ~ mmkh_ms7_min$new_p, ylab="mmkh sen's slope")

dev.off()

monthly_q=c()
for ( i in 1:12) monthly_q[i] =paste0(str_to_lower(month.abb[i]),"_med_q")
mmky_par(raw_data = monthly_q)
remove(monthly_q)

monthly_q=c()
for ( i in 1:12) monthly_q[i] =paste0(str_to_lower(month.abb[i]),"_mn_q")
mmky_par(raw_data = monthly_q)

monthly_t= c()
for ( i in 1:12) monthly_t[i] =paste0(str_to_lower(month.abb[i]),"_mn_t")
mmky_par(raw_data = monthly_t)

monthly_med_t= c()
for ( i in 1:12) monthly_med_t[i] =paste0(str_to_lower(month.abb[i]),"_med_t")
mmky_par(raw_data = monthly_med_t)

monthly_p= c()
for ( i in 1:12) monthly_p[i] =paste0(str_to_lower(month.abb[i]),"_sm_p")
mmky_par(raw_data = monthly_p)

monthly_pet= c()
for ( i in 1:12) monthly_pet[i] =paste0(str_to_lower(month.abb[i]),"_pet")
mmky_par(raw_data = monthly_pet)


seasonal_dy_drought=c()
vec= c("summer", "spring", "winter")
for ( i in 1:3) seasonal_dy_drought[i] =paste0(vec[i],"_dy_drought_q")
mmky_par(raw_data = seasonal_dy_drought)

seasonal_dy_drought=c()
for ( i in 1:3) seasonal_dy_drought[i] =paste0(vec[i],"_sm_def_q")
mmky_par(raw_data = seasonal_dy_drought)

for ( i in 1:3) seasonal_dy_drought[i] =paste0(vec[i],"_dy_drought_p")
mmky_par(raw_data = seasonal_dy_drought)


for ( i in 1:3) seasonal_dy_drought[i] =paste0(vec[i],"_sm_def_p")
mmky_par(raw_data = seasonal_dy_drought)

remove(monthly_q, monthly_t,monthly_p,monthly_pet,monthly_med_t,seasonal_dy_drought,monthly_sum_def, vec)

#precip trend without the years 1971, 1976,2003
data_x = yearly_sm_p[-c(2,7,34),]
mmky_par(raw_data = "data_x")
remove(data_x)

#precip trend without the 70s
yr_sm_p_no70 = yearly_sm_p[-c(1:10),]
mmky_par(raw_data = "yr_sm_p_no70")


#why NAs? ####
#problem of sqrt(VS) = na produced 
which(is.na(mmkh_ms7_min$new_p ))

plot(ms7_min[,229], t="p", ylim=c(0,1))
points(ms7_min[,70], col=2)

mmkh(ms7_min[,229])

mmky(ms7_min[,229])
  which.max(gauges$q_mean)

mkttest(ms7_min[,229])

#problem occurs in the essf part of the function it is negative, which is negative because of rof, which becomes negative if the acf at lag one is outside the 95 confidence interval
#with no AR correction ####

# mk_jun = sapply(c(jun_mean_df[,1:catch_n]), FUN= mkttest) %>% t()



  


 