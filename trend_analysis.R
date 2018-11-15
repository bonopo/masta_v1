
# trend analysis ----------------------------------------------------------


#plotting mann-kendall ####

mmkh_winter_q10 = as.data.frame(mmkh_winter_q10)

colnames(mmkh_winter_q10) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var")

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
  geom_point(aes(y=mmkh_summer_q10$tau , x=mmkh_winter_q10$tau, col=as.factor(gauges_df$sr)))+
  geom_abline(slope = 1, intercept =  0)+
  ylab("summer q10 mmkh tau")+
  xlab("winter q10 mmkh tau")+
  scale_color_discrete("Season of \n low flow", label=c("summer", "unclear", "winter"))

ggsave("winter_summer_q10.png")
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

mmkh_ms7_min
mmkh_ms7_min
neg_tau_ms7 =   which(mmkh_ms7_min$tau <0 & mmkh_ms7_min$new_p < .05) 
  

neg_q10 = which(mmkh_yearly_q10$Tau < 0)



#monthly trend analysis ####

for ( i in 1:12) res[i] =paste0("mmkh_",str_to_lower(month.abb[i]),"_mean_df")

monthly_mmkh_tau = get(res[1:12])$tau

monthly_mmkh_tau = sapply(1:12, function(x) get(res[x])$tau) %>% as.data.frame()

png("monthly_mmkh_bxplt.png")
boxplot(monthly_mmkh_tau, names = month.abb, ylab="mmkh tau of monthly mean" )
dev.off()
