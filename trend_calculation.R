# Trend analysis ----------------------------------------------------------

source("./R/masta_v1/functions.R")# has to run before if not objects will be missing!
source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missing!


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
mk_tests_par(raw_data = "jun_mean_df")
mk_tests_par(raw_data = c("mar_mean_df","jul_mean_df"))


#problem of sqrt(VS) = na produced
mmkh_par(raw_data = c("jun_mean_df", "mar_mean_df", "ms7_date", "ms7_min", "ms30_min", "yearly_q10","summer_q10", "winter_q10"))

mmkh_par("summer_q10")


for ( i in 1:12) res[i] =paste0(str_to_lower(month.abb[i]),"_mean_df")

mmkh_par(raw_data = res)




#with no AR correction ####

mk_jun = sapply(c(jun_mean_df[,1:catch_n]), FUN= mkttest) %>% t()
