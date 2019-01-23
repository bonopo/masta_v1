
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
#"p_days_of_drought_yr" ,"q_days_of_drought_yr","p_sum_def_yr","q_sum_def_yr"
#"march_dy_drought_q", "march_dy_drought_p","march_sm_def_p","march_sm_def_q","june_dy_drought_q", "june_dy_drought_p","june_sm_def_p","june_sm_def_q"
mmky_mar_mn_q$sen_slope[gauges$sr_new==2 & mmky_mar_mn_q$new_p<0.05]  %>% range()

p=sig_plot(x_data = "mmky_ms7_min", y_data = "mmky_yearly_7_min", output = "sr_new", p_value =.05) 
p+
geom_abline(slope=1, intercept=0)
  ggsave(plot = p, "./plots/trend_analysis/ms7_sig_7min.png")

plot(su_sm_p$`100`)
abline(a= median(su_sm_p$`100`), b = mmky_su_sm_p$sen_slope[100])
which.min(mmky_su_sm_p$sen_slope)
40*mmky_su_sm_p$sen_slope[100]

which(mmky_mar_mn_q$sen_slope > 0 & mmky_mar_mn_q$new_p < 0.05 & gauges$sr_new == 0) %>%  length() /length(which(gauges$sr_new == 0))

yearly_sm_p$`21` %>% plot(type="l")
gauges$lt_memoryeffect
p = catch_plot(p_value=.05, color="mn_deficit", x_data="lt_memoryeffect", y_data= "mmky_ms30_min" , factor =F)
p
p= p + xlab("SAAR [mm]")+
  ylab("ms30 trend (slope) [m³/s/a]")+
  scale_color_discrete("ln mean deficit [m³]",labels = c("9-11", "11-13","13-15"))
p

ggsave(plot = p, "./plots/trend_analysis/ms30_mean_def.png")


p_value=.05; color="saar"; x_data="cor_spi_n"; y_data= "mmky_q10" 
z_value = dplyr::select(gauges_df, color)[,1]
ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value)], x=dplyr::select(gauges_df, x_data)[which(get(y_data)$new_p<p_value),], col=z_value[which(get(y_data)$new_p<p_value)]), position = position_dodge(width = .5))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value ))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(x_data)+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("saar")



x_data = "mmky_mar_mn_q";y_data = "mmky_jun_mn_q"
ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=mmky_wi_p_pet$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("Hydro Geo.")


p_value=.05; color="sr_new";x_data="hydrogeo_simple"; y_data= "mmky_ms30_min" ;gauges_df = gauges %>% as.data.frame();z_value = as.factor(dplyr::select(gauges_df, color)[,1])


ggplot()+
  geom_boxplot( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value)], x=dplyr::select(gauges_df, x_data)[which(get(y_data)$new_p<p_value),], col=z_value[which(get(y_data)$new_p<p_value)]), position = "dodge")+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value ))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab("Hydrogeology")+
  ylab("ms30 trend (slope) [m³/s/a]")+
  scale_color_discrete("Seasonality",labels = c("Summer", "Winter"))

ggsave("./plots/5_choice/hydrogeology.pdf")


which(mmky_yearly_7_min$sen_slope[mmky_yearly_7_min$new_p<.05] >0 & gauges$sr_new[mmky_yearly_7_min$new_p<.05] == 2) %>% length()
#10 winter lf catchments have a neg trend for the whole year and 6 a positive
#if only looking at summer (ms7) 10 have a negative and 7 a positve
#both at significance level 0.05
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


ms7_min_med = apply(ms7_min, 1, median)
yearly_max_t_mean = apply(yearly_max_t, 1, mean)
rects <- data.frame(xstart = seq(1969.5,2008.5,1), 
                    xend = seq(1970.5,2009.5,1), 
                col = yearly_max_t_mean)
ggplot() + geom_point(aes(x=1970:2009, y=ms7_min_med)) +
           geom_rect(data=rects, aes(ymin=0, ymax=.5, 
                                     xmin=xstart,
                      xmax=xend, fill=col), alpha =0.5)+
 scale_fill_continuous("mean yearly max T [°C]", low="blue", high = "red")+
  xlab("")+
  ylab("median ms7 [m³/s]")
  
ggsave("./plots/5_choice/max_temp_med_ms7.png")
