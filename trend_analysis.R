
# trend analysis ----------------------------------------------------------

#plotting sen slope ####
#per catchment characteristic
  p = catch_plot(p_value=1, color="sr_new", x_data="bfi", y_data= "winter", factor =T)
p


pos.neg(dat= mmky_wi_sm_p, p = 0.03, positive=T) 
magnitude(dat = mmky_wi_ext_p)*40

mmky_sp_mn_t[gauges$sr_new==0,]$sen_slope %>% mean

p= p+ylab("ms30 trend (slope) [m³/s/a]")+
  xlab("BFI")+
  scale_color_discrete("Seasonality" , labels =c("Summer","Winter"))
p
ggsave(plot = p, "./plots/trend_analysis/su_mn_t_hochwert.png")

#trend vs trend
winter = seasonal_trends(lb_season=12, ub_season=2, dat = mt_mn_temp, value="temp_m", xtable = F, px=0.03)
spring = seasonal_trends(lb_season=3, ub_season=5, dat = mt_mn_temp, value="temp_m", xtable = F, px=0.03)

summer = seasonal_trends(lb_season=6, ub_season=8, dat = mt_mn_temp, value="temp_m", xtable = F, px=0.03)
autumn = seasonal_trends(lb_season=9, ub_season=11, dat = mt_mn_temp, value="temp_m", xtable = F, px=0.03)


p=sig_plot(x_data = "summer", y_data = "winter", output = "hochwert", p_value =fs_su_mn_t) 
p


p+
#geom_abline(slope=1, intercept=0)+
  geom_point(aes(x=mmky_su_mn_t$sen_slope[gauges$alpine == 1], y= mmky_wi_mn_t$sen_slope[gauges$alpine == 1]),col="red")+
  xlab("summer mean temperature trend (slope) [°C/a]")+
    ylab("winter mean temperature trend (slope) [°C/a]")+
  scale_color_continuous("Latitude")+
  theme_bw()
  ggsave("./plots/trend_analysis/winter_summer_temp.pdf")
  
  
  ggplot()+
geom_point(aes(x=mmky_su_q10$sen_slope, y=mmky_wi_q10$sen_slope, color=as.factor(gauges$sr_new)))+
  scale_color_discrete("Regime", labels =c("pluvial","nival"))+
  xlab(expression("summer "*Q[10]*" trend (slope) [m³/s/a]"))+
  ylab(expression("winter "*Q[10]*" trend (slope) [m³/s/a]"))+
    theme_bw()+
    nice

ggsave("./plots/trend_analysis/quantile_trend.pdf", width=19, height=14, unit="cm")


ggplot()+
geom_point(aes(x=mmky_wi_med_q$sen_slope, y=mmky_wi_sm_p$sen_slope, color=as.factor((mmky_wi_med_q$new_p < fs_wi_mn_q) & mmky_wi_sm_p$new_p < fs_wi_sm_p)))+
  scale_color_discrete("Significant", labels =c("False","True"))+
  xlab("winter sum precipitation trend (slope) [mm/a]")+
  ylab("winter median q trend (slope) [m³/s/a]")+
  theme_bw()
  ggsave( "./plots/trend_analysis/wi_p_med_q_sr.png")

ggplot()+
  geom_point(aes(x=gauges$bfi, y=mmky_ms30_min$sen_slope, col=as.factor(gauges$sr_new)))+
  scale_color_discrete("Seasonality", labels =c("Summer","Winter"))+
  ylab("ms30 trend (slope) [m³/s/a]")+
  xlab("BFI")
ggsave("./plots/trend_analysis/bfi_ms30.pdf")
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



#trend analysis with sen's slope####

# "ms7_date", "ms7_min", "ms30_min", "yearly_q10","yearly_mn_q","su_q10", "wi_q10", "su_mn_t", "wi_mn_t","yearly_mn_t", "yearly_max_t", "yearly_sm_p",    "su_sm_p", "wi_sm_p"
#"p_days_of_drought_yr" ,"q_days_of_drought_yr","p_sum_def_yr","q_sum_def_yr"
#"march_dy_drought_q", "march_dy_drought_p","march_sm_def_p","march_sm_def_q","june_dy_drought_q", "june_dy_drought_p","june_sm_def_p","june_sm_def_q"



#other plots####

data_plot = cbind.data.frame(sr=gauges$sr_new, precip_wi  = mmky_wi_sm_p$sen_slope*40/gauges$wi_sm_p)

ggplot(data_plot)+
  geom_boxplot(aes(x=as.factor(sr), y=precip_wi))

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

x_data = "mmky_mw7_min";y_data = mmky_ms7_min[gauges$sr_new == 2,]
ggplot()+
  geom_point( aes(y=(y_data)$sen_slope[which((y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which((y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$saar[which((y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which((y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
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

ggsave("./plots/trend_analysis/hydrogeology_ms30.pdf")


which(mmky_yearly_7_min$sen_slope[mmky_yearly_7_min$new_p<.05] >0 & gauges$sr_new[mmky_yearly_7_min$new_p<.05] == 2) %>% length()
#10 winter lf catchments have a neg trend for the whole year and 6 a positive
#if only looking at summer (ms7) 10 have a negative and 7 a positve
#both at significance level 0.05
#trend linear regression ####
#----> see script drought_attribution

#ms7timing plot

data_plot = cbind.data.frame(ms7 = mmky_ms7_date$sen_slope, sr=gauges$sr_new) 
which()


ggplot(data_plot,aes(x=as.factor(sr), y=ms7))+
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  labs(x="seasonality", 
       y= expression(T[Q[7]]*" [d/a]"))+
  scale_x_discrete(labels=c("pluvial","summer"))+
  theme_bw()
ggsave("./plots/trend_analysis/sr_ms7_timing.pdf", width=10, height=10, unit="cm")

#trend moving window  #### 
yr_sm_p_sbst = mmky_sbst(raw_data = yearly_sm_p, width=30)
ms7_sbst = mmky_sbst(raw_data = ms7_min, width=10)
ms7_sbst30 = mmky_sbst(raw_data = ms7_min, width=30)
subset_yr_sm_p = mmky_sbst(raw_data = yearly_sm_p, width = 30)

plot(subset_yr_sm_p$`1970`, type="p")
points (subset_yr_sm_p$`1979`, col=2)
plot(data=as.matrix(subset_yr_sm_p), `1970` ~ `1979`)
abline(a=0, b=1)

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
 # filter(gauge %in% neg_ms7) %>% 
  group_by(gauge = as.integer(gauge)) %>% 
  summarise(lb=min(mmky), ub=max(mmky)) %>% 
  mutate(mmky_true = mmky_ms7_min$sen_slope)# %>% 
  mutate(gauge = 1:length(neg_ms7))

data_plot2 =ms7_sbst30_long %>% 
    #filter(gauge %in% neg_ms7) %>% 
   mutate(gauge= rep(1:catch_n, times=10))


ggplot(data_plot, aes(x=gauge, fill=as.numeric(year), y=mmky))+
  #geom_point(aes(y= mmky_ms7_min$sen_slope, x=gauges$saar))
  geom_bar(position = "fill", stat = "identity", width = 10)+
  scale_color_continuous("Starting year")

ggplot(data= data_plot)+
  geom_pointrange(aes(x=gauge,y=mmky_true, ymin=lb, ymax=ub), size=.2, linetype = 1, pch=3)+
  ylab("ms7 trend (slope) [m³/s/a]")
ggsave("./plots/trend_analysis/30_year_moving_window.pdf")

up_mean = data_plot$ub - data_plot$mmky_true %>% as.vector 
up_mean %>% mean
  
late_mean = ms7_sbst30_long %>% 
  filter(year == 1979) %>% 
  mutate(mean = mmky - mmky_ms7_min$sen_slope)

early_mean = ms7_sbst30_long %>% 
  filter(year == 1970) %>% 
  mutate(mean = mmky - mmky_ms7_min$sen_slope)

hist(late_mean$mmky)
t.test(x= late_mean$mmky, y = early_mean$mmky)
t.test(x= ms7_sbst30_long %>% 
  filter(year == 1979) %>% 
    dplyr::select(mmky) ,
  y= ms7_sbst30_long %>% 
  filter(year == 1970) %>% 
    dplyr::select(mmky))

lp_mean = data_plot$mmky_true - data_plot$lb %>% as.vector 
lp_mean %>% mean

min=  apply(yr_sm_p_sbst,1,min)
max=  apply(yr_sm_p_sbst,1,max)
p = catch_plot(p_value=10, color="sr_new", x_data="saar", y_data= "mmky_yearly_sm_p" , factor =T)
p+geom_linerange(aes(x=gauges$saar, ymin = min, ymax= max), alpha=.3)
ggsave("./plots/trend_analysis/yr_sm_p_30yr_subset.png")  
data_plot2 %<>% as.tbl()

ggplot()+
  geom_point(data= data_plot2, aes(x=gauge, y=mmky, color=as.numeric(year)))+
  geom_point(data=data_plot, aes(x=gauge, y= mmky_true), pch=3, col=2)+
  scale_color_continuous("Year")+
  xlab("all neg. sen's slope catchments with p<.05")
ggsave("./30_year_moving_window2.png")


yearly_p_pet = year_p_pet %>% 
  mutate(date=date_seq) %>% 
  gather(., value=p_pet,key=catchment,-date ) %>% 
  as.tbl() %>% 
  mutate(catchment = as.integer(catchment)) %>% 
  group_by(year(date), catchment) %>% 
  summarise(yearly_sum = sum(p_pet)) %>% 
  mutate(year = `year(date)` %>% as.integer()) %>% 
  group_by(year) %>% 
  summarise(year_med = median(yearly_sum))

ms7_min_med = apply(ms7_min, 1, median)
yearly_max_t_mean = apply(yearly_max_t, 1, median)#
rects <- data.frame(xstart = seq(1969.5,2008.5,1), 
                    xend = seq(1970.5,2009.5,1), 
                col = yearly_p_pet$year_med)
pet_plot = ggplot() + geom_point(aes(x=1970:2009, y=ms7_min_med)) +
           geom_rect(data=rects, aes(ymin=0, ymax=.5, 
                                     xmin=xstart,
                      xmax=xend, fill=col), alpha =0.5)+
 scale_fill_continuous("P-PET [mm/a] ", low="red", high = "blue", labels =c("0","250","500"), breaks=c(0,250,500))+
  xlab("")+
  ylab(expression(Q[min[7]]*" [m³/s]"))+nice
  
#ggsave("./plots/trend_analysis/p_pet_med_ms7.pdf")

yearly_p = mt_sm_p_wide%>% 
  mutate(date=date_seq) %>% 
  gather(., value=p,key=catchment,-date ) %>% 
  group_by(year(date)) %>% 
  summarise(year_med = median(p))

ms7_min_med = apply(ms7_min, 1, median)
yearly_max_t_mean = apply(yearly_max_t, 1, median)#
rects <- data.frame(xstart = seq(1969.5,2008.5,1), 
                    xend = seq(1970.5,2009.5,1), 
                col = yearly_max_t_mean)
temp=ggplot() + geom_point(aes(x=1970:2009, y=ms7_min_med)) +
           geom_rect(data=rects, aes(ymin=0, ymax=.5, 
                                     xmin=xstart,
                      xmax=xend, fill=col), alpha =0.5)+
 scale_fill_continuous("max temp. [°C]", low="blue", high = "red")+
  xlab("")+
  ylab("")+nice
  

p= grid.arrange(pet_plot,temp,  ncol=2)
ggsave(plot=p,"./plots/trend_analysis/barcode_plot.pdf", width = 18, height=11, unit="cm")



#monthly boxplots ####
#monthly q10 plot
mmky_monthly_q10= c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_q10[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_q10")
data_plot[[i]]  = (get(mmky_monthly_q10[i])$sen_slope*40)/gauges$mn_q
sr_new_l[[i]] = gauges$sr_new
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                            )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  # stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #                position = position_dodge(width = 1), size =3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly q10trend (slope) [m³/s/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
  ggsave("./plots/trend_analysis/monthly_q10.png")
  


#monthly q35 plot
mmky_monthly_q35 = c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_q35[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_q35")
data_plot[[i]]  = (get(mmky_monthly_q35[i])$sen_slope*40)/gauges$mn_q
sr_new_l[[i]] = gauges$sr_new
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                            )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  # stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #                position = position_dodge(width = 1), size =3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly q35 trend (slope) [m³/s/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
  ggsave("./plots/trend_analysis/monthly_q35.png")
  

#monthly precip trends
mmky_monthly_p = c()
data_plot=list()
sr_new_l=list()
#check = list()
for(i in 1:12) {
  mmky_monthly_p[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_sm_p")
data_plot[[i]]  = get(mmky_monthly_p[i])$sen_slope[get(mmky_monthly_p[i])$new_p < 0.03] 
sr_new_l[[i]] = gauges$sr_new[get(mmky_monthly_p[i])$new_p < 0.03] 
#check[[i]]= which(get(mmky_monthly_p[i])$new_p < fs_sm_p[i])
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
 stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                position = position_dodge(width = 1), size =3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly precipitation trend (slope) [mm/a]")+
  scale_color_discrete("Seasonality", labels=c("no-snow","snow-acc"))
  ggsave("./plots/trend_analysis/monthly_p_sr_sig.pdf")
  
#non significant  
mmky_monthly_p = c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_p[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_sm_p")
data_plot[[i]]  = get(mmky_monthly_p[i])$sen_slope*40/gauges$saar *100
sr_new_l[[i]] = gauges$sr_new
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                            )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  # stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #                position = position_dodge(width = 1), size =3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("precip. trend (slope) [%/40a]")+
  scale_color_discrete("Regime", labels=c("pluvial","nival"))+theme_bw()+
  nice
  ggsave("./plots/trend_analysis/monthly_p_sr_non_sig.pdf", width=19, height = 14, unit="cm")
  

#monthly pet trends
mmky_monthly_pet = c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_pet[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_pet")
data_plot[[i]]  = get(mmky_monthly_pet[i])$sen_slope
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length))
               
                          )

#summer
ggplot(d,aes(x = as.factor(grp), y = x)) + 
  geom_boxplot()+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("PET trends (slope) [mm/a]")+
  nice
  #scale_color_discrete("Seasonality", labels=c("summer","winter"))#+
  ggsave("./plots/trend_analysis/monthly_pet.pdf")

#winter
ggplot(d[d$col == 2,],aes(x = as.factor(grp), y = x)) + 
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly PET[mm] trends (slope) [mm/a]")
  #scale_color_discrete("Seasonality", labels=c("summer","winter"))#+
  ggsave("./plots/trend_analysis/monthly_pet_winter.pdf")


#non significant pet trends

mmky_monthly_pet = c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_pet[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_pet")
data_plot[[i]]  = get(mmky_monthly_pet[i])$sen_slope
sr_new_l[[i]] = gauges$sr_new
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d[d$col == 0,],aes(x = as.factor(grp), y = x)) + 
  geom_boxplot()+
  stat_summary(fun.data = give.n.summer, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly PET trends (slope) [mm/a]")+
  #scale_color_discrete("Seasonality", labels=c("summer","winter"))#+
  ggsave("./plots/trend_analysis/monthly_pet_summer.pdf")


#p-PET
mmky_monthly_p_pet = c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_p_pet[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_p_pet")
data_plot[[i]]  = get(mmky_monthly_p_pet[i])$sen_slope[get(mmky_monthly_p_pet[i])$new_p < fs_mt_p_pet[i]]
sr_new_l[[i]] = gauges$sr_new[get(mmky_monthly_p_pet[i])$new_p < fs_mt_p_pet[i]]
check[[i]]= which(get(mmky_monthly_p_pet[i])$new_p < fs_mt_p_pet[i])
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l),
                check= unlist(check)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly sum p[mm] - PET[mm] trend (slope) [mm/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))#+
  ggsave("./plots/trend_analysis/monthly_p_pet_sr.png")
  
#with all (not just significant trends)  P-PET
mmky_monthly_p_pet = c()
data_plot=list()
sr_new_l=list()
check = list()
for(i in 1:12) {
  mmky_monthly_p_pet[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_p_pet")
data_plot[[i]]  = get(mmky_monthly_p_pet[i])$sen_slope
sr_new_l[[i]] = gauges$sr_new
}
names(data_plot) = str_to_lower(month.abb)
boxplot(data_plot)

d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
               #   position = position_dodge(width = 0.75))+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("P-PET trend (slope) [mm/a]")+
  scale_color_discrete("Regime", labels=c("pluvial","nival"))+
  nice
  ggsave("./plots/trend_analysis/monthly_p_pet_sr_non_sig.pdf")

remove(mmky_monthly_p_pet, check, sr_new_l, d, data_plot)

#monthly  sens slope Q
#mean
mmky_monthly_q = c()
data_plot=list()
sr_new_l=list()
for(i in 1:12) {
  mmky_monthly_q = paste0("mmky_",str_to_lower(month.abb[i]),"_mn_q")
data_plot[[i]]  = get(mmky_monthly_q)$sen_slope[get(mmky_monthly_q)$new_p < fs_mt_q[i]]
sr_new_l[[i]] = gauges$sr_new[get(mmky_monthly_q)$new_p < fs_mt_q[i]]
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 1), size=3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly mean q trend (slope) [m³/s/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  ggsave("./plots/trend_analysis/monthly_q_sr.pdf")


ggplot(d,aes(x = as.factor(grp), y = x)) + #without grouping
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly mean q trend (slope) [m³/s/a]")+
 # scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  ggsave("./plots/trend_analysis/monthly_q.pdf")

remove(mmky_monthly_p_pet, check, sr_new_l, d, data_plot)

#median
mmky_monthly_q = c()
data_plot=list()
sr_new_l=list()
for(i in 1:12) {
  mmky_monthly_q = paste0("mmky_",str_to_lower(month.abb[i]),"_med_q")
data_plot[[i]]  = get(mmky_monthly_q)$sen_slope[get(mmky_monthly_q)$new_p < fs_mt_q[i]]
sr_new_l[[i]] = gauges$sr_new[get(mmky_monthly_q)$new_p < fs_mt_q[i]]
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
 # stat_summary( fun.data = give.n, geom = "text", fun.y = median,
  #                position = position_dodge(width = .75), size=3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly median q trend (slope) [m³/s/a]")+
 scale_color_discrete("Seasonality", labels=c("no-snow","snow-acc"))+
  theme_bw()
  ggsave("./plots/trend_analysis/monthly_med_q_sr.pdf")


ggplot(d,aes(x = as.factor(grp), y = x)) + 
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 0.75))+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly median q trend (slope) [m³/s/a]")+
 # scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  ggsave("./plots/trend_analysis/monthly_med_q.pdf")

remove(mmky_monthly_p_pet, check, sr_new_l, d, data_plot)

#median non significant
mmky_monthly_q = c()
data_plot=list()
sr_new_l=list()
for(i in 1:12) {
  mmky_monthly_q = paste0("mmky_",str_to_lower(month.abb[i]),"_med_q")
data_plot[[i]]  = get(mmky_monthly_q)$sen_slope
sr_new_l[[i]] = gauges$sr_new
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("Q trend (slope) [m³/s/a]")+
 scale_color_discrete("Regime", labels=c("pluvial","nival"))+
  theme_bw()+nice
  ggsave("./plots/trend_analysis/monthly_med_q_sr_perc.pdf", width=21, height=15, unit="cm")

  
# q quantile plot line
  data_plot=list()

  for(i in 1:12) {
  mmky_monthly_q = paste0("mmky_",str_to_lower(month.abb[i]),"_med_q")
data_plot[[i]]  = get(mmky_monthly_q)$sen_slope
}
  d_med <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length))
                ) %>% 
    group_by(grp) %>% 
    summarise(med_med =median(x))
  
  #q10
  for(i in 1:12) {
  mmky_monthly_q10[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_q10")
data_plot[[i]]  = get(mmky_monthly_q10[i])$sen_slope
}


d_q10 <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length))
                
                            )%>% 
    group_by(grp) %>% 
    summarise(med_q10 =median(x))

 for(i in 1:12) {
  mmky_monthly_q35[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_q35")
data_plot[[i]]  = get(mmky_monthly_q35[i])$sen_slope

}


d_q35 <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length))
                
                            )%>% 
    group_by(grp) %>% 
    summarise(med_q35 =median(x))

#q80
mmky_monthly_q80  = c()

for(i in 1:12) {
  mmky_monthly_q80[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_q80")
data_plot[[i]]  = get(mmky_monthly_q80[i])$sen_slope

}


d_q80 <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length))
                
                            )%>% 
   group_by(grp) %>% 
    summarise(med_q80 =median(x))

data_plot = cbind.data.frame(q50 = d_med$med_med, q10 = d_q10$med_q10, q35= d_q35$med_q35, q80 = d_q80$med_q80) %>% 
  mutate(month=1:12) %>% 
  gather(key=Quantile, value= median,-month)
  
ggplot(data_plot)+
    geom_line(aes(x=month, y=median, col=Quantile))+
  scale_x_continuous("",breaks=c(1:12),labels=c(month.abb[1:12]))+
  ylab("Q trend (slope) [m³/s/a]")+
  theme_bw()+
  scale_color_discrete(labels = c(expression(Q[10]),expression(Q[35]),expression(Q[50]),expression(Q[80])))+
  nice
ggsave("./plots/trend_analysis/quantile_q.pdf", width = 20, height=15, unit="cm")
#mean t monthly trends
mmky_monthly_t = c()
data_plot=list()
sr_new_l=list()
fs = 10

for(i in 1:12) {
  mmky_monthly_t = paste0("mmky_",str_to_lower(month.abb[i]),"_mn_t")
data_plot[[i]]  = get(mmky_monthly_t)$sen_slope[get(mmky_monthly_t)$new_p < fs]
sr_new_l[[i]] = gauges$sr_new[get(mmky_monthly_t)$new_p < fs]
}


d <- data.frame(x = unlist(data_plot)*40, 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #                position = position_dodge(width = 1), size=3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("T trend (slope) [°C/40a]")+
  scale_color_discrete("Regime", labels=c("pluvial","nival"))+
  theme_bw()+nice
  ggsave("./plots/trend_analysis/monthly_t_sr.pdf")

 dat=d %>% 
    group_by(grp) %>% 
    summarise(med =median(x))
 values <- dat$med
ii <- cut(values, breaks = seq(min(values), max(values), len = 100), 
          include.lowest = TRUE) 
colors = colorRampPalette(c(2,"white",4))(99)[ii]


RColorBrewer::brewer.pal(n=3, "RdBu")
  doughnut( rep(1,12) , labels=(month.abb), inner.radius=0.5, col=c(colors), clockwise = T ,  lty=3, density = NULL)
  
 dat$count=rep(1,12)
dat$fraction = dat$count / sum(dat$count)
dat$ymax = cumsum(dat$fraction)
dat$ymin = c(0, head(dat$ymax, n=-1))

ggplot(dat, aes(fill=med, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
     geom_rect() +
     coord_polar(theta="y") +
     xlim(c(0, 4)) +
     theme(panel.grid=element_blank()) +
     theme(axis.text=element_blank()) +
     theme(axis.ticks=element_blank()) +
     annotate("text", x = 0, y = 0, label = "My Ring plot !") +
     labs(title="")

# Make the plot
ggplot(dat, aes(fill=med, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
     geom_rect() +
     coord_polar(theta="y") +
     xlim(c(0, 4)) +
     theme(panel.grid=element_blank()) +
     theme(axis.text=element_blank()) +
     theme(axis.ticks=element_blank()) +
     annotate("text", x = 0, y = 0, label = "My Ring plot !") +
     labs(title="")


#monthly median trend
mmky_monthly_med_t = c()
data_plot=list()
sr_new_l=list()
for(i in 1:12) {
  mmky_monthly_med_t = paste0("mmky_",str_to_lower(month.abb[i]),"_mn_t")
data_plot[[i]]  = get(mmky_monthly_med_t)$sen_slope[get(mmky_monthly_med_t)$new_p < fs_mn_t[i]]
sr_new_l[[i]] = gauges$sr_new[get(mmky_monthly_med_t)$new_p < fs_mn_t[i]]
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  stat_summary(fun.data = give.n, geom = "text", fun.y = median,
                  position = position_dodge(width = 1), size=3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly mean t trend (slope) [°C/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
  ggsave("./plots/trend_analysis/monthly_t_sr.pdf")

#why is april so different?
data_plot = apr_mn_t %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=mn_t, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            as.tbl

ggplot(data_plot)+ #loess
  geom_smooth(aes(x=year, y=mn_t), method = "loess", span=.1)

ggplot(data_plot)+
  geom_line(aes(x=year, y=mn_t,col=as.factor(gauge)), show.legend = F)

#with iqr and median not loess
data_plot = apr_mn_t %>% 
            mutate(year = 1970:2009) %>% 
            gather(value=mn_t, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            mutate(sr=rep(gauges$sr_new, times=40) )%>% 
            group_by(year,sr) %>% 
            summarise(yr_med = median(mn_t), sd_neg=quantile(mn_t,.25), sd_pos=quantile(mn_t,.75)) %>% 
            as.tbl

ggplot(data_plot %>% filter(sr==0))+
  geom_line(aes(x=year, y=yr_med), col=2)+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col=2)+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col=2)+
  ylab("april mean temperature [°C]")
ggsave("./plots/trend_analysis/apr_med_iqr_t_summer.png")

ggplot(data_plot %>% filter(sr==2))+
  geom_line(aes(x=year, y=yr_med), col=4)+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col=4)+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col=4)+
  ylab("april mean temperature [°C]")
ggsave("./plots/trend_analysis/apr_med_iqr_t_winter.png")

#is march or may similar?
data_plot = mar_mn_t %>% #march
            mutate(year = 1970:2009) %>% 
            gather(value=mn_t, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            group_by(year) %>% 
            summarise(yr_med = median(mn_t), sd_neg=quantile(mn_t,.25), sd_pos=quantile(mn_t,.75)) %>% 
            as.tbl

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med))+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed")+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed")+
  ylab("march mean temperature [°C]")
ggsave("./plots/trend_analysis/march_t_med.png")

data_plot = may_mn_t %>% #may
            mutate(year = 1970:2009) %>% 
            gather(value=mn_t, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            group_by(year) %>% 
            summarise(yr_med = median(mn_t), sd_neg=quantile(mn_t,.25), sd_pos=quantile(mn_t,.75)) %>% 
            as.tbl

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med))+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed")+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed")+
  ylab("may mean temperature [°C]")
ggsave("./plots/trend_analysis/may_t_med.png")

#is it better if median is used instead of mean to calculate monthly values
data_plot = apr_med_t %>% #may
            mutate(year = 1970:2009) %>% 
            gather(value=mn_t, key=gauge, -year) %>% 
            mutate(gauge=as.integer(gauge)) %>% 
            group_by(year) %>% 
            summarise(yr_med = median(mn_t), sd_neg=quantile(mn_t,.25), sd_pos=quantile(mn_t,.75)) %>% 
            as.tbl

ggplot(data_plot)+
  geom_line(aes(x=year, y=yr_med))+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed")+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed")+
  ylab("may temperature [°C]")
ggsave("./plots/trend_analysis/may_t_med.png")


#mean p_pet values (not trend)
data_plot = spei_data %>% 
  group_by(month(yr_mt), gauge) %>% 
  summarise( mean(p_pet)) %>% 
  as.tbl()
  data_plot$sr =  rep(gauges$sr_new, times = 12)

ggplot(data_plot,aes(x = as.factor(`month(yr_mt)`), y = `mean(p_pet)`,col=as.factor(sr)))+
  geom_boxplot()+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("P-PET [mm/month]")+
  scale_color_discrete("Seasonality", labels=c("pluvial","nival"))+
  nice
  ggsave("./plots/trend_analysis/monthly_p-pet_sr.pdf")

#mean pet values (not trend)
data_plot = spei_data %>% 
  group_by(month(yr_mt), gauge) %>% 
  summarise( mean(pet_th)) %>% 
  as.tbl()
  data_plot$sr =  rep(gauges$sr_new, times = 12)

ggplot(data_plot,aes(x = as.factor(`month(yr_mt)`), y = `mean(pet_th)`,col=as.factor(sr)))+
  geom_boxplot()+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("PET [mm/month]")+
  scale_color_discrete("Seasonality", labels=c("pluvial","nival"))+nice
  ggsave("./plots/trend_analysis/monthly_pet_sr.pdf")

#monthly precip
data_plot = mt_sm_p %>% 
  group_by(x= month(yr_mt), gauge) %>% 
  summarise(y= mean(month_sum)) %>% 
  mutate(sr = gauges$sr_new %>% as.integer()) %>% 
  as.tbl()
  

ggplot(data_plot,aes(x = as.factor(x), y = y, col=as.factor(sr)))+
  geom_boxplot()+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("P [mm/month]")+
  scale_color_discrete("Seasonality", labels=c("pluvial","nival"))+nice
  ggsave("./plots/trend_analysis/monthly_mn_sm_p.pdf")


data_plot2 = data_plot %>%
  set_colnames(.,1:12) %>% 
  gather(., key=month, value=slope) %>% 
  mutate(month=as.integer(month)) %>% 
  as.tbl
ggplot(data=data_plot2, aes(x= as.factor(month), y = slope ))+ 
  geom_boxplot()

#mean monthly temp (not trend)
data_plot = mt_mn_temp %>% 
  group_by(x= month(yr_mt), gauge) %>% 
  summarise(y= mean(temp_m)) %>% 
  mutate(sr = gauges$sr_new %>% as.integer()) %>% 
  as.tbl()
  

ggplot(data_plot,aes(x = as.factor(x), y = y, col=as.factor(sr)))+
  geom_boxplot()+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly mean temperature [°C]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  ggsave("./plots/trend_analysis/monthly_mn_t_sr.png")


#winter and summer sum p
p= ggplot()+
    geom_point(aes(x= gauges$saar, y = mmky_wi_sm_p$sen_slope*40/gauges$wi_sm_p*100, col=as.factor(gauges$sr_new)))

error_x = error.bar("wi_sm_p")
p+ geom_linerange(alpha= .3,aes( ymin=low_lim*40/gauges$wi_sm_p*100, ymax = upp_lim*40/gauges$wi_sm_p*100 ,x=gauges$saar), data = error_x)+
  theme_bw()+
   scale_color_discrete("Regime", labels=c("pluvial","nival"))+nice+
  xlab("SAAR [mm/a]")+
  ylab("winter precipitation sum trend (slope) [%/40a]")
ggsave("./plots/trend_analysis/wi_sm_p_conf.pdf")

p= ggplot()+
    geom_point(aes(x= gauges$saar, y = mmky_yearly_sm_p$sen_slope*40/gauges$saar*100, col=as.factor(mmky_yearly_sm_p$new_p < fs_yr_sm_p)))

error_x = error.bar("yearly_sm_p")
p+ geom_linerange(alpha= .3,aes( ymin=low_lim*40/gauges$saar*100, ymax = upp_lim*40/gauges$saar*100 ,x=gauges$saar), data = error_x)+
  theme_bw()+
  scale_color_discrete("Regime", labels=c("pluvial","nival"))+
  xlab("SAAR [mm/a]")+
  ylab("yearly precip. sum trend (slope) [%/40a]")+
  nice
ggsave("./plots/trend_analysis/yr_sm_p_conf.pdf")

#now as boxplot or barplot
p= ggplot()+
    geom_point(aes(x= gauges$saar, y = mmky_su_sm_p$sen_slope*40/gauges$wi_sm_p*100, col=as.factor(gauges$sr_new)))

error_x = error.bar("su_sm_p")
hist(gauges$saar)
error_x= error_x*40/gauges$su_sm_p*100
error_x$saar = cut(gauges$saar, breaks = c(0,700,800,900,1100,1400,1800),labels=c("<700","700-800","800-900","900-1100", "1100-1400",">1400"))


data_plot = error_x %>% 
  group_by(saar) %>% 
  summarise(ub = mean(upp_lim), lb=mean(low_lim), sen=mean(sen_dat))


ggplot(data_plot, aes(x= saar, y=sen))+
  geom_point(size=1.3)+
  geom_errorbar(aes(ymin=lb, ymax=ub, x=saar),width=0.2, size=1, col="grey", alpha=.5)+
  ylab("summer P trend [%/40a]")+
  scale_x_discrete("ARS [mm]")+
  theme_bw()

ggsave("./plots/trend_analysis/summer_conf.pdf")

p+ geom_linerange(alpha= .3,aes( ymin=low_lim*40/gauges$su_sm_p*100, ymax = upp_lim*40/gauges$su_sm_p*100 ,x=gauges$saar), data = error_x)+
  theme_bw()+
   scale_color_discrete("Regime", labels=c("pluvial","nival"))+nice+
  xlab("SAAR [mm/a]")+
  ylab("summer precipitation sum trend (slope) [%/40a]")
ggsave("./plots/trend_analysis/wi_sm_p_conf.pdf")

#winter sum p with IQR and median
data_plot = wi_sm_p %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sm_p,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times=40)) %>% 
  group_by(year, sr) %>% 
  summarise(yr_med = median(sm_p), sd_neg=quantile(sm_p,.25), sd_pos=quantile(sm_p,.75)) %>% 
    as.tbl

data_plot %>% dplyr::select(sm_p) %>% hist #not normal so I can not just look at the +/-2*sd


ggplot(data_plot %>% filter(sr==0))+
  geom_line(aes(x=year, y=yr_med), col=2)+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col=2)+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col=2)+
  ylab("winter precipitation sum [mm/a]")
ggsave("./plots/trend_analysis/wi_med_iqr_summer.png")

ggplot(data_plot %>% filter(sr==2))+
  geom_line(aes(x=year, y=yr_med), col=4)+
  geom_line(aes(x=year, y=sd_neg),linetype="dashed", col=4)+
  geom_line(aes(x=year, y=sd_pos),linetype="dashed", col=4)+
  ylab("winter precipitation sum [mm/a]")
ggsave("./plots/trend_analysis/wi_med_iqr_winter.png")

#loess method
data_plot = wi_sm_p %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sm_p,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

ggplot(data_plot)+
         geom_smooth(aes(x=year, y=sm_p, col=sr), method = "loess", span=0.25)+
  ylab("winter precipitation sum [mm/year]")+
    scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  xlab("")+
  ggsave("./plots/statistical/wi_sm_p_loess.png")

#summer sum p with loess
data_plot = su_sm_p %>% 
  mutate(year = 1970:2009) %>% 
  gather(., key=gauge, value=sm_p,-year) %>% 
  mutate(sr=rep(gauges$sr_new, times = 40)) %>% 
  as.tbl

ggplot(data_plot)+
         geom_smooth(aes(x=year, y=sm_p, col=sr), method = "loess", span=0.25)+
  ylab("summer precipitation sum [mm/year]")#+
    scale_color_discrete("Seasonality", labels=c("summer","winter"))+
  xlab("")+
  ggsave("./plots/statistical/su_sm_p_loess.png")
    
    
    #monthly box plots of drought characteristics
    
mmky_sum_def = c()
data_plot=list()
sr_new_l=list()
check = list()
fs= 0.02967359
for(i in 1:12) {
  mmky_sum_def[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_sum_drought_q")
data_plot[[i]]  = get(mmky_sum_def[i])$sen_slope[get(mmky_sum_def[i])$new_p < fs]
sr_new_l[[i]] = gauges$sr_new[get(mmky_sum_def[i])$new_p < fs]
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #               position = position_dodge(width = 1), size =3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly precipitation trend (slope) [mm/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
  ggsave("./plots/trend_analysis/monthly_p_sr.png")
  #no trends
  
  #days of drought
  mmky_days_dr = c()
data_plot=list()
sr_new_l=list()
check = list()
fs= 0.02967359
fs=3
for(i in 1:12) {
  mmky_days_dr[i] = paste0("mmky_",str_to_lower(month.abb[i]),"_dy_drought_q")
data_plot[[i]]  = get(mmky_days_dr[i])$sen_slope[get(mmky_days_dr[i])$new_p < fs]
sr_new_l[[i]] = gauges$sr_new[get(mmky_days_dr[i])$new_p < fs]
}


d <- data.frame(x = unlist(data_plot), 
                grp = rep(1:12,times = sapply(data_plot,length)),
                col = unlist(sr_new_l)
                )
ggplot(d,aes(x = as.factor(grp), y = x, col=as.factor(col))) + 
  geom_boxplot()+
  #stat_summary(fun.data = give.n, geom = "text", fun.y = median,
  #               position = position_dodge(width = 1), size =3)+
  scale_x_discrete(labels= c(month.abb))+
  xlab("")+
  ylab("monthly precipitation trend (slope) [mm/a]")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
  ggsave("./plots/trend_analysis/monthly_p_sr.png")
  #no trend
    
  
  #summer precip
  
  ggplot()+
    geom_point(aes(x= gauges$saar, y=((mmky_su_sm_p$sen_slope/gauges$saar)*40), col=as.factor(gauges$sr_new)))+
      ylab("sumer sum precipitation trend (slope) [%/40a]")+
  xlab("SAAR [mm]")+
scale_color_discrete("Seasonality" , labels =c("Summer","Winter"))
  ggsave( "./plots/trend_analysis/su_sm_p_non_sig.pdf")
  
#error bars####
sig_points = which(mmky_yearly_sm_p$new_p < fs_yr_sm_p  )
error_x = error.bar("yearly_sm_p")
  p = catch_plot(p_value=fs_yr_sm_p, color="sr_new", x_data="saar", y_data= "mmky_yearly_sm_p" , factor =T)
p+ geom_linerange(alpha= .4,aes( ymin=low_lim, ymax = upp_lim ,x=gauges$saar[sig_points]), data = error_x[sig_points,])
ggsave("./plots/trend_analysis/yr_sm_p_error.png")

#comparing time span  ####
 #for streamflow
int = which(ymd(gauges$Ztrhnbg)<ymd("1950-1-2"))
#data not available.....

#for temperature
load("./data/catchments/eobs_temp_part.Rdata", verbose = T)
tempera = tempera[,c(1:catch_n)]
colnames(tempera) <- 1:catch_n
temp_long <- load_file(file=tempera, value_name = "temp", origin = "1950-01-01")

#measuring seasonal temperature increase
summer = temp_long %>% 
  filter(between(month(date),6,8)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
summer$`year(date)` #time series goes from 1950 - 2015
summer = summer[,2:(catch_n+1)]

autumn = temp_long %>% 
  filter(between(month(date),9,11)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
autumn = autumn[,2:(catch_n+1)]

winter = temp_long %>% 
  filter(between(month(date),9,11)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
winter = winter[,2:(catch_n+1)]

spring = temp_long %>% 
  filter(between(month(date),9,11)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
spring = spring[,2:(catch_n+1)]

mmky_par(c("summer", "spring", "winter", "autumn"))

#now from the shortened period

summer_short = temp_long %>% 
  filter(between(year(date),1970,2009)) %>% 
  filter(between(month(date),6,8)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
summer_short$`year(date)` #time series goes from 1950 - 2015
summer_short = summer_short[,2:(catch_n+1)]

autumn_short = temp_long %>% 
  filter(between(year(date),1970,2009)) %>% 
  filter(between(month(date),9,11)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
autumn_short = autumn_short[,2:(catch_n+1)]

winter_short = temp_long %>% 
  filter(between(year(date),1970,2009)) %>% 
  filter(between(month(date),9,11)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
winter_short = winter_short[,2:(catch_n+1)]

spring_short = temp_long %>% 
  filter(between(year(date),1970,2009)) %>% 
  filter(between(month(date),9,11)) %>% 
  group_by(year(date), gauge) %>% 
  summarise(mean_t = mean(temp)) %>% 
  spread(key=gauge, value=mean_t ) %>% 
  as.data.frame() 
spring_short = spring_short[,2:(catch_n+1)]

mmky_par(c("summer_short", "spring_short", "winter_short", "autumn_short"))



