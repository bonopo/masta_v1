
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
#seasonal mk test ####
ken_summer_min_q = ken_trend(data_source = "summer_min_q", sci=FALSE)
gauges$ken_summer_min_q =ken_summer_min_q[,1]

gauges$summer_ave_q = ken_trend(data_source = "summer_ave_q", sci=FALSE)[,1]
gauges$summer_sum_p = ken_trend(data_source = "summer_sum_p", sci=FALSE)[,1]
gauges$summer_q_q10 = ken_trend(data_source = "summer_q_q10", sci=FALSE)[,1]



#mann kendall with AR correction ####

mk_tests_par(raw_data = c("dr_freq_5yr", "dr_freq_10yr"))

#problem of sqrt(VS) = na produced

mmkh_ms7_min = t(sapply(c(ms7_min[,1:ncol(ms7_min)]), FUN =mmkh))
mmkh_ms7_date = t(sapply(c(ms7_date[,1:ncol(ms7_date)]), FUN =mmkh)) %>% as.data.frame()
mmkh_ms7_min %<>% as.data.frame() 

for ( i in 1:12) res[i] =paste0(str_to_lower(month.abb[i]),"_mean_df")
mmkh_par(raw_data = c(res))





# mann- kendall test ------------------------------------------------------


ken_spei <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), data_source =  "spei_", sci = TRUE)
ken_spi <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), sci = TRUE, data_source =  "spi_")
ken_ssi <- ken_trend(data_source =  "ssi_1", sci = FALSE )


# quantil trend ####


quant_trend_1 <- qua_trend(quantil = 0.1, data_source = "q_long")
quant_trend_05 <- qua_trend(quantil = 0.05, data_source = "q_long") # wie schweizer defnition Q347

pdf("./plots/mk_quant.pdf")
plot(quant_trend_05$tau, ylab="tau", xlab="catchments", ylim=c(-0.65, .45))
points(quant_trend_1$tau, col=2)
legend("bottomleft", pch=c(1,1), col=c(1,2), c("quantil = .05", "quantil = .1"), bty="n")
abline(h=0, lty=2, col=4)
dev.off()

