
# Trend analysis ----------------------------------------------------------

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")

# mann- kendall test ------------------------------------------------------


ken_spei <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), data_source =  "spei_", sci = TRUE)
ken_spi <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), sci = TRUE, data_source =  "spi_")
ken_ssi <- ken_trend(data_source =  "ssi_1", sci = FALSE )

mnq30_df = mnq30[,c(1:catch_n+1)]
ken_mnq30 = ken_trend(data_source = "mnq30_df", sci=FALSE)
gauges$mnq30_yearly_trend = ken_mnq30[,1]

plot(ken_ssi[,1])#, log="y",)

# quantil trend ####


quant_trend_1 <- qua_trend(quantil = 0.1, data_source = "q_long")
quant_trend_05 <- qua_trend(quantil = 0.05, data_source = "q_long") # wie schweizer defnition Q347

pdf("./plots/mk_quant.pdf")
plot(quant_trend_05$tau, ylab="tau", xlab="catchments", ylim=c(-0.65, .45))
points(quant_trend_1$tau, col=2)
legend("bottomleft", pch=c(1,1), col=c(1,2), c("quantil = .05", "quantil = .1"), bty="n")
abline(h=0, lty=2, col=4)
dev.off()


# seasonal trends ####
summer_ave_q = seas_cl(data_source = "mt_mn_q", method = "mean", value = "q_mean", begin =4, end=10) 
summer_min_q = seas_cl(data_source = "mt_mn_q", method = "min", value = "q_mean", begin =4, end=10) #summer mnq30

summer_sum_p = seas_cl(data_source = "mt_sm_p", method = "sum", value = "month_sum")
winter_sum_p = seas_cl(data_source = "mt_sm_p", method = "sum", value = "month_sum", begin = 11, end =3)
summer_q_q10 =  mt_mn_q %>% 
  filter(month(yr_mt) >= 4, month(yr_mt)<= 10) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(q10 = quantile(q_mean, .1)) %>% 
  ungroup() %>% 
  spread(key=gauge, value=q10) %>% 
  as.data.frame()


ken_summer_min_q = ken_trend(data_source = "summer_min_q", sci=FALSE)
gauges$ken_summer_min_q =ken_summer_min_q[,1]

gauges$summer_ave_q = ken_trend(data_source = "summer_ave_q", sci=FALSE)[,1]
gauges$summer_sum_p = ken_trend(data_source = "summer_sum_p", sci=FALSE)[,1]
gauges$summer_q_q10 = ken_trend(data_source = "summer_q_q10", sci=FALSE)[,1]

#yearly trends ####
yearly_mean_q = mt_mn_q %>% 
  group_by(gauge, year) %>% 
  summarise(yearly_mean = mean(q_mean))


