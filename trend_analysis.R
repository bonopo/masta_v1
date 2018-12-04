
# trend analysis ----------------------------------------------------------


#plotting mann-kendall ####

mmkh_winter_q10 = as.data.frame(mmkh_winter_q10)

colnames(mmkh_winter_q10) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var")


data_bb = as.data.frame(bb_jun_mean_df)
plot(unlist(data_bb$`Sen's slope`)~mmkh_jun_mean_df$sen_slope)
      plot(mmkh_jun_mean_df$corrected_z  ~ mmkh_jun_mean_df$new_p)
which(mmkh_jun_mean_df$new_p < .05) %>% length()

 # plot(mmkh_summer_ave_q_df$Tau ~ mmkh_summer_min_q_df$)

ggsave("q_mean_mk_nq7.png")

ggplot()+
  geom_point(data = data_bb, aes(y=new_tau, x=as.factor(gauges$mnq30_month), col= gauges$bfi),inherit.aes = FALSE)+
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
  neg_ms7 =   which(mmkh_ms7_min$sen_slope <0 & mmkh_ms7_min$new_p < .05) 
  

neg_q10 = which(mmkh_yearly_q10$Tau < 0)



#monthly trend analysis ####
res=c()
for ( i in 1:12) res[i] =paste0("mmkh_",str_to_lower(month.abb[i]),"_mean_df")




monthly_mmkh_sen = sapply(1:12, function(x) get(res[x])$sen_slope[which(get(res[x])$new_p <.1)]) 
#monthly_mmkh_sen = sapply(1:12, function(x) get(res[x])$sen_slope) 

png("./plots/further_investigate/monthly_mmkh_bxplt.png", width=1000, height = 500)
boxplot(monthly_mmkh_sen, names = month.abb, ylab="mmkh significant sen's slope of monthly mean" )
dev.off()

monthly_mmkh_aov = sapply(1:12, function(x) get(res[x])$sen_slope[which(get(res[x])$new_p <.1)]) 

 aov_data = matrix(ncol=2, nrow=0)
 for (i in 1:12){
   aov_data = rbind(aov_data, cbind(monthly_mmkh_aov[[i]], rep(i, times=length(monthly_mmkh_aov[[i]]))))
  }  
aov_data %<>% as.data.frame()
colnames(aov_data)= c("sen_slope", "month")
head(aov_data)
aov(sen_slope ~ month, data=aov_data)%>% summary()

#trend analysis with sen's slope####






# "ms7_date", "ms7_min", "ms30_min", "yearly_q10","yearly_mn_q","su_q10", "wi_q10", "su_mn_t", "wi_mn_t","yearly_mn_t", "yearly_max_t", "yearly_sm_p",    "su_sm_p", "wi_sm_p"

p=sig_plot(y_data = "mmky_ms7_date", x_data = "mmky_su_sm_p", output = "sr", p_value = 5) 
p
ggsave(plot = p, "./plots/3_choice/mmky_laaha_defi_defi_saar.pdf")



ggplot()+
  geom_point(aes(y= mmky_tot_defi_q$sen_slope[which(mmky_tot_defi_q$new_p < .05)], x=gauges$saar[which(mmky_tot_defi_q$new_p < .05)], col=as.factor(gauges$sr_new[which(mmky_tot_defi_q$new_p < .05)])))+
  scale_color_discrete("hydrogeo")

#trend linear regression ####
#----> see script drought_attribution

#trend moving window  #### 

ms7_sbst = mmky_sbst(raw_data = ms7_min, width=10)
ms7_sbst30 = mmky_sbst(raw_data = ms7_min, width=30)

res= lapply(1:catch_n, FUN= function(x) summary(lm(as.numeric(ms7_sbst[x,])~c(1:30)))$coefficients[,c(1,4)])


hist(as.numeric(ms7_sbst30[2,]))
plot(mmky_ms7_min$sen_slope)
plot(y=ms7_sbst30[1,], x=1:10, t="l")

sum(ms7_sbst30[2,], na.rm=T)/ncol(ms7_sbst30)
mean(as.numeric(ms7_sbst30[2,]))
mmky_ms7_min$sen_slope[2]
ms7_sbst_long = ms7_sbst %>% 
  mutate(gauge= as.numeric(rownames(ms7_sbst))) %>% 
  gather(key=year, value=mmky, -gauge)

ms7_sbst30_long = ms7_sbst30 %>% 
  mutate(gauge= as.numeric(rownames(ms7_sbst30))) %>% 
  gather(key=year, value=mmky, -gauge)

data_plot = ms7_sbst30_long %>% 
  filter(gauge %in% neg_ms7) %>% 
  group_by(gauge = as.integer(gauge)) %>% 
  summarise(lb=min(mmky), ub=max(mmky)) %>% 
  mutate(mmky_true = mmky_ms7_min[neg_ms7,]$sen_slope) %>% 
  mutate(gauge = 1:length(neg_ms7))

data_plot2 =ms7_sbst30_long %>% 
    filter(gauge %in% neg_ms7) %>% 
   mutate(gauge= rep(1:length(neg_ms7), times=10))

ggplot(data_plot, aes(x=gauge, fill=as.numeric(year), y=mmky))+
  #geom_point(aes(y= mmky_ms7_min$sen_slope, x=gauges$saar))
  geom_bar(position = "fill", stat = "identity", width = 10)+
  scale_color_continuous("Starting year")

ggplot(data= data_plot)+
  geom_pointrange(aes(x=gauge,y=mmky_true, ymin=lb, ymax=ub))+
  ylab("mmky")
ggsave("./30_year_moving_window.png")
  
data_plot2 %<>% as.tbl()

ggplot()+
  geom_point(data= data_plot2, aes(x=gauge, y=mmky, color=as.numeric(year)))+
  geom_point(data=data_plot, aes(x=gauge, y= mmky_true), pch=3, col=2)+
  scale_color_continuous("Year")+
  xlab("all neg. sen's slope catchments with p<.05")
ggsave("./30_year_moving_window2.png")


geom_line

head(data_plot2)


