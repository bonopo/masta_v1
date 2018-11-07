
# Trend analysis ----------------------------------------------------------

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")



#seasonal mk test ####
ken_summer_min_q = ken_trend(data_source = "summer_min_q", sci=FALSE)
gauges$ken_summer_min_q =ken_summer_min_q[,1]

gauges$summer_ave_q = ken_trend(data_source = "summer_ave_q", sci=FALSE)[,1]
gauges$summer_sum_p = ken_trend(data_source = "summer_sum_p", sci=FALSE)[,1]
gauges$summer_q_q10 = ken_trend(data_source = "summer_q_q10", sci=FALSE)[,1]


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


mk_tests_par(raw_data = c("yearly_min_q","summer_ave_q","summer_min_q","summer_q_q10","nq_monthly", "yearly_mean_q"))


mk_tests_par(raw_data = c("ms30_min", "ms30_date"))


#problem of sqrt(VS) = na produced

mmkh_summer_q10 = t(sapply(c(summer_q_q10[,1:ncol(summer_q_q10)]), FUN =mmkh))


# mann- kendall test ------------------------------------------------------


ken_spei <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), data_source =  "spei_", sci = TRUE)
ken_spi <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), sci = TRUE, data_source =  "spi_")
ken_ssi <- ken_trend(data_source =  "ssi_1", sci = FALSE )

mnq30_df = mnq30[,c(1:catch_n+1)]
ken_mnq30 = ken_trend(data_source = "mnq30_df", sci=FALSE)
gauges$mnq30_yearly_trend = ken_mnq30[,1]

plot(ken_ssi[,1])#, log="y",)

which.min(ken_mnq30[,1])

#plotting mann-kendall ####

data_mmkh = as.data.frame(mmkh_yearly_q10)

colnames(data_mmkh) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "Tau", "sen_slope", "old_var", "new_var")

data_bb = modiscloud::unlist_df(bb_ms7_min_df)
plot(mmkh_summer_ave_q_df$Tau ~ mmkh_summer_min_q_df$Tau)

ggsave("q_mean_mk_nq7.png")

ggplot()+
  geom_point(data = data_mmkh, aes(y=Tau, x=as.factor(gauges$mnq30_month), col= gauges$bfi),inherit.aes = FALSE)+
  xlab("month of mnq30")+
  ylab("mk tau of yearly q10")+
  scale_color_continuous("BFI")

ggplot()+
  geom_point(data = data_mmkh, aes(y=Tau, col=gauges$bfi, x=gauges$Enzgsg_),inherit.aes = FALSE)+
  xlab("catchment size [km²]")+
  ylab("mk tau of yearly q10")+
  scale_color_continuous("BFI")

ggplot()+
  geom_point(data = data_mmkh, aes(y=Tau, x=gauges$saar),inherit.aes = FALSE)+ 
  geom_point(data= data_mmkh[which(data_mmkh$new_p<.05),] , aes(y=Tau, x=gauges$saar[which(data_mmkh$new_p<.05)], col="p<0.05"))+
  xlab("SAAR [mm]")+
  ylab("mk tau of yearly q10")+
  scale_color_discrete("Significance")

ggplot()+
  geom_point(data = data_bb, aes(y=tau, x=gauges$q_mean ,col=gauges$cor_spi))+
  xlab("q mean [unit???]")+
  ylab("mk tau of nq7 summer")+
  scale_color_continuous("Correlation of \n SPI-n")


ggplot()+
  geom_point(data = data_bb[1:50,], aes(y=tau, x=gauges$saar[1:50], col=gauges$bfi[1:50]),inherit.aes = FALSE)+
 geom_errorbar(data =data_bb[1:50,], aes(x=gauges$saar[1:50], ymin= lb , ymax= ub), colour=2, width = .05,inherit.aes = FALSE)

ggplot()+
  geom_point(data = data_bb[1:50,], aes(y=tau, x=1:50),inherit.aes = FALSE)+
 geom_errorbar(data =data_bb[1:50,], aes(x=1:50, ymin= lb , ymax= ub), colour=2, width = .05,inherit.aes = FALSE)


ggplot()+
  geom_point(data = data_bb[1:50,], aes(y=tau, x=1:50),inherit.aes = FALSE)


# quantil trend ####


quant_trend_1 <- qua_trend(quantil = 0.1, data_source = "q_long")
quant_trend_05 <- qua_trend(quantil = 0.05, data_source = "q_long") # wie schweizer defnition Q347

pdf("./plots/mk_quant.pdf")
plot(quant_trend_05$tau, ylab="tau", xlab="catchments", ylim=c(-0.65, .45))
points(quant_trend_1$tau, col=2)
legend("bottomleft", pch=c(1,1), col=c(1,2), c("quantil = .05", "quantil = .1"), bty="n")
abline(h=0, lty=2, col=4)
dev.off()


# trends in drought charachteristics --------------------------------------

#looking only at the catchments that have a negative trend in q10 values:

neg_q10 = which(mmkh_yearly_q10$Tau < 0)

trend_dsi= sapply(1:catch_n,  FUN = function(x) MannKendall(dsi_1_yearly[[x]]$mean_dsi)) %>% t() %>% as.data.frame() %>% modiscloud::unlist_df()
trend_len_mmkh=sapply(1:catch_n,  FUN = function(x) mmkh(dsi_1_yearly[[x]]$mean_length)) %>% t() %>% as.data.frame() %>% modiscloud::unlist_df()
trend_inten = sapply(1:catch_n,  FUN = function(x) MannKendall(dsi_1_yearly[[x]]$mean_inten)) %>% t() %>% as.data.frame() %>% modiscloud::unlist_df()

ggsave("q10_dsi_trend.png")

ggplot()+
  geom_point(aes(x=trend_len$tau, y=mmkh_yearly_q10$Tau))+
  geom_point(data = trend_len[trend_len$sl<.1,], aes(x=tau, y=mmkh_yearly_q10$Tau[which(trend_len$sl<.1)], col="p<0.1"))+
  ylab("mmkh tau q10 yearly")+
  xlab("mk tau drought length")+
  scale_color_discrete("Significance of \n drought length tau")

ggplot()+
  geom_point(aes(x=trend_len$tau, y=mmkh_yearly_q10$Tau))+
    geom_point(data = mmkh_yearly_q10[which(mmkh_yearly_q10$`new P-value`<0.05 && trend_len$sl<0.05),] , aes(x=trend_len$tau[which(mmkh_yearly_q10$`new P-value`<0.05)], y=Tau, col="p<0.05"))+
  ylab("mmkh tau q10 yearly")+
  xlab("mk tau drought length")+
  scale_color_discrete("Significance of \n drought length tau")

ggplot()+
  geom_point(aes(x=trend_inten$tau, y=mmkh_yearly_q10$Tau))+
    geom_point(data = mmkh_yearly_q10[which(mmkh_yearly_q10$`new P-value`<0.1),], aes(x=trend_inten$tau[which(mmkh_yearly_q10$`new P-value`<0.1)], y=Tau, col="p<0.1"))+
  ylab("mmkh tau q10 yearly")+
  xlab("mk tau drought intensity")+
  scale_color_discrete("Significance of \n q10 trend")

gauges$spi_n_cor


plot(trend_len$sl ~ trend_len_mmkh$new.P.value)

gauges$mmkh_q10 = mmkh_yearly_q10$Tau
gauges$mmkh_q10[gauges$mmkh_q10 <= -.2] = -.2
gauges$mmkh_q10[gauges$mmkh_q10 < 0 & gauges$mmkh_q10 > -.2] = -.1
gauges$mmkh_q10[gauges$mmkh_q10 >= 0 & gauges$mmkh_q10 < .2] = 0
gauges$mmkh_q10[gauges$mmkh_q10 >= .2] = .2

gauges$mmkh_q10 = cut(gauges$mmkh_q10, seq(-.3, .3,.1 )) 

hist(gauges$mmkh_q10)

labelat = c(0,2,3,4,5,6)
labeltext = c("low","high","low","high","low","high")
spplot(gauges, c("mmkh_q10"), col.regions = rainbow(100, start = 4/6, end = 1),
    colorkey = list(
        labels=list(
            at = labelat,
            labels = labeltext
        )
    )
)
# negative trend detail examination ####


neg_tau_q10 = mmkh_yearly_q10 %>% 
  as.data.frame() %>% 
  filter(Tau <0)

neg_q10 = which(mmkh_yearly_q10$Tau < 0)


