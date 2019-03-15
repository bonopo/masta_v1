#xtable script for LaTeX tables
#generel low flow trends
mmky_ms7_date_su = mmky_ms7_date[gauges$sr_new ==2,]

rowname_table = c("ms30", "ms7", "ms7 timing")

mat = matrix(nrow= length(rowname_table), ncol=7, data = 
               c(
                 pos.neg(p=NULL, dat = mmky_ms30_min, positive = T),
                 pos.neg(p=fs_ms30, dat = mmky_ms30_min, positive = T),
                 pos.neg(p=NULL, dat = mmky_ms30_min, positive = F),
                 pos.neg(p=fs_ms30, dat = mmky_ms30_min, positive = F),
                 round(fs_ms30,2),
                 magnitude(dat = mmky_ms30_min, reference = gauges$mn_q),
                 pos.neg(p=NULL, dat = mmky_ms7_min, positive = T),
                 pos.neg(p=fs_ms7, dat = mmky_ms7_min, positive = T),
                 pos.neg(p=NULL, dat = mmky_ms7_min, positive = F),
                 pos.neg(p=fs_ms7, dat = mmky_ms7_min, positive =F),
                 round(fs_ms7,2),
                 magnitude(dat = mmky_ms7_min, 
                           reference = gauges$mn_q),
                 pos.neg(p=NULL, dat = mmky_ms7_date[gauges$sr_new ==2,],
                         positive = T),
                 pos.neg(p=fs_ms7_date, dat = mmky_ms7_date[gauges$sr_new ==2,], positive = T),
                 pos.neg(p=NULL, dat = mmky_ms7_date[gauges$sr_new ==2,], positive = F),
                 pos.neg(p=fs_ms7_date, dat = mmky_ms7_date[gauges$sr_new ==2,], positive = F),
                 round(fs_ms7_date,2),
                 magnitude(dat = mmky_ms7_date[gauges$sr_new ==2,])
                 ),               
             byrow=T) %>% as.data.frame()
colname_table = c(rep(c("ns","s"),2),"field significance", "range","")
rownames(mat)= rowname_table
colnames(mat) = colname_table
xtable::xtable(mat, digits=c(0,0,0,0,0,2,2,2))

#temp.table 
rowname_table = c("yearly", "winter", "spring","summer","days below 0")

mat = matrix(nrow= length(rowname_table), ncol=7, data = 
               c(
                 pos.neg(p=NULL, dat = mmky_yearly_mn_t, positive = T),
                 pos.neg(p=fs_yr_mn_t, dat = mmky_yearly_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_yearly_mn_t, positive = F),
                 pos.neg(p=fs_yr_mn_t, dat = mmky_yearly_mn_t, positive = F),
                 round(fs_yr_mn_t,2),
                 magnitude(dat = mmky_yearly_mn_t)*40,
                 pos.neg(p=NULL, dat = mmky_wi_mn_t, positive = T),
                 pos.neg(p=fs_wi_mn_t, dat = mmky_wi_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_wi_mn_t, positive = F),
                 pos.neg(p=fs_wi_mn_t, dat = mmky_wi_mn_t, positive =F),
                 round(fs_wi_mn_t,2),
                 magnitude(dat = mmky_wi_mn_t)*40,
                 pos.neg(p=NULL, dat = mmky_sp_mn_t, positive = T),
                 pos.neg(p=fs_sp_mn_t, dat = mmky_sp_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_sp_mn_t, positive = F),
                 pos.neg(p=fs_sp_mn_t, dat = mmky_sp_mn_t, positive = F),
                 round(fs_sp_mn_t,2),
                 magnitude(dat = mmky_sp_mn_t)*40,
                 pos.neg(p=NULL, dat = mmky_su_mn_t, positive = T),
                 pos.neg(p=fs_su_mn_t, dat = mmky_su_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_su_mn_t, positive = F),
                 pos.neg(p=fs_su_mn_t, dat = mmky_su_mn_t, positive = F),
                 round(fs_su_mn_t,2),
                 magnitude(dat = mmky_su_mn_t)*40,
                 pos.neg(p=NULL, dat = mmky_wi_days_below_0, positive = T),
                 pos.neg(p=fs_wd_0, dat = mmky_wi_days_below_0, positive = T),
                 pos.neg(p=NULL, dat = mmky_wi_days_below_0, positive = F),
                 pos.neg(p=fs_wd_0, dat = mmky_wi_days_below_0, positive = F),
                 round(fs_wd_0,2),
                 magnitude(dat = mmky_wi_days_below_0)*40
                 ),               
             byrow=T) %>% as.data.frame()
colname_table = c(rep(c("ns","s"),2),"field significance", "range","")
rownames(mat)= rowname_table
colnames(mat) = colname_table


#precip.table
rowname_table = c("yearly", "winter", "summer")

mat = matrix(nrow= length(rowname_table), ncol=7, data = 
               c(
                   pos.neg(p=NULL, dat = mmky_yearly_sm_p, positive = T),
                 pos.neg(p=fs_yr_sm_p, dat = mmky_yearly_sm_p, positive = T),
                 pos.neg(p=NULL, dat = mmky_yearly_sm_p, positive = F),
                 pos.neg(p=fs_yr_sm_p, dat = mmky_yearly_sm_p, positive = F),
                 round(fs_yr_sm_p,2),
                 range(mmky_yearly_sm_p$sen_slope*40/gauges$saar)*100 %>% round(.,2),
                 pos.neg(p=NULL, dat = mmky_wi_sm_p, positive = T),
                 pos.neg(p=fs_wi_sm_p, dat = mmky_wi_sm_p, positive = T),
                 pos.neg(p=NULL, dat = mmky_wi_sm_p, positive = F),
                 pos.neg(p=fs_wi_sm_p, dat = mmky_wi_sm_p, positive =F),
                 round(fs_wi_sm_p,2),
                 range(mmky_wi_sm_p$sen_slope*40/gauges$wi_sm_p)*100 %>% round(.,2),
                 pos.neg(p=NULL, dat = mmky_su_sm_p, positive = T),
                 pos.neg(p=fs_su_sm_p, dat = mmky_su_sm_p, positive = T),
                 pos.neg(p=NULL, dat = mmky_su_sm_p, positive = F),
                 pos.neg(p=fs_su_sm_p, dat = mmky_su_sm_p, positive = F),
                 round(fs_su_sm_p,2),
                 range(mmky_su_sm_p$sen_slope*40/gauges$wi_sm_p)*100 %>% round(.,2)
                 ),               
             byrow=T) %>% as.data.frame()


pos.neg(p=fs_dwr, dat= mmky_days_without_rain, positive=T)
magnitude(dat= mmky_days_without_rain)*40
#drought trends

rowname_table = c("length", "deficit", "frequency")

mat = matrix(nrow= length(rowname_table), ncol=10, data = 
               c(
                   pos.neg(p=NULL, dat = mmky_p_days_of_drought_yr, positive = T),
                 pos.neg(p=fs_dd_p, dat = mmky_p_days_of_drought_yr, positive = T),
                 pos.neg(p=NULL, dat = mmky_p_days_of_drought_yr, positive = F),
                 pos.neg(p=fs_dd_p, dat = mmky_p_days_of_drought_yr, positive = F),
                 round(fs_dd_p,2),
                  pos.neg(p=NULL, dat = mmky_q_days_of_drought_yr, positive = T),
                 pos.neg(p=fs_dd_q, dat = mmky_q_days_of_drought_yr, positive = T),
                 pos.neg(p=NULL, dat = mmky_q_days_of_drought_yr, positive = F),
                 pos.neg(p=fs_dd_q, dat = mmky_q_days_of_drought_yr, positive = F),
                 round(fs_dd_q,2),
                 
                   pos.neg(p=NULL, dat = mmky_p_sum_def_yr, positive = T),
                 pos.neg(p=fs_ds_p, dat = mmky_p_sum_def_yr, positive = T),
                 pos.neg(p=NULL, dat = mmky_p_sum_def_yr, positive = F),
                 pos.neg(p=fs_ds_p, dat = mmky_p_sum_def_yr, positive = F),
                 round(fs_ds_p,2),
                  pos.neg(p=NULL, dat = mmky_q_sum_def_yr, positive = T),
                 pos.neg(p=fs_ds_q, dat = mmky_q_sum_def_yr, positive = T),
                 pos.neg(p=NULL, dat = mmky_q_sum_def_yr, positive = F),
                 pos.neg(p=fs_ds_q, dat = mmky_q_sum_def_yr, positive = F),
                 round(fs_ds_q,2),
                 
                   pos.neg(p=NULL, dat = mmky_p_n_events_yr, positive = T),
                 pos.neg(p=fs_df_p, dat = mmky_p_n_events_yr, positive = T),
                 pos.neg(p=NULL, dat = mmky_p_n_events_yr, positive = F),
                 pos.neg(p=fs_df_p, dat = mmky_p_n_events_yr, positive = F),
                 round(fs_df_p,2),
                  pos.neg(p=NULL, dat = mmky_q_n_events_yr, positive = T),
                 pos.neg(p=fs_df_q, dat = mmky_q_n_events_yr, positive = T),
                 pos.neg(p=NULL, dat = mmky_q_n_events_yr, positive = F),
                 pos.neg(p=fs_df_q, dat = mmky_q_n_events_yr, positive = F),
                 round(fs_df_q,2)
                 
                 ),               
             byrow=T) %>% as.data.frame()

colname_table = c(rep(c("+","+","-","-","\alpha"),2))
rownames(mat)= rowname_table
colnames(mat) = colname_table
xtable::xtable(mat, digits=c(0,rep(c(0,0,0,0,2),2)))

#extreme precip
mat = matrix(nrow= 3, ncol=6, data = 
               c(pos.neg(dat = mmky_yearly_ext_p, positive = T),
  pos.neg(dat = mmky_yearly_ext_p, positive = T, p = fs_ext_precip),
  pos.neg(dat = mmky_yearly_ext_p, positive = F),
  pos.neg(dat = mmky_yearly_ext_p, positive = F, p = fs_ext_precip),
  magnitude(dat= mmky_yearly_ext_p)*40/,
  
  pos.neg(dat = mmky_wi_ext_p, positive = T),
  pos.neg(dat = mmky_wi_ext_p, positive = T, p = fs_ext_precip),
  pos.neg(dat = mmky_wi_ext_p, positive = F),
  pos.neg(dat = mmky_wi_ext_p, positive = F, p = fs_ext_precip),
  magnitude(dat= mmky_wi_ext_p),
  pos.neg(dat = mmky_su_ext_p, positive = T),
  pos.neg(dat = mmky_su_ext_p, positive = T, p = fs_ext_precip),
  pos.neg(dat = mmky_su_ext_p, positive = F),
  pos.neg(dat = mmky_su_ext_p, positive = F, p = fs_ext_precip),
  magnitude(dat= mmky_su_ext_p)
   ),               
             byrow=T) %>% as.data.frame()
mat
xtable::xtable(mat)

#days without rain
mat = matrix(nrow= 3, ncol=6, data = 
               c(pos.neg(dat = mmky_yearly_dwr, positive = T),
  pos.neg(dat = mmky_yearly_dwr, positive = T, p = fs_ext_precip),
  pos.neg(dat = mmky_yearly_dwr, positive = F),
  pos.neg(dat = mmky_yearly_dwr, positive = F, p = fs_ext_precip),
  magnitude(dat= mmky_yearly_dwr)*40,
  
  pos.neg(dat = mmky_wi_dwr, positive = T),
  pos.neg(dat = mmky_wi_dwr, positive = T, p = fs_ext_precip),
  pos.neg(dat = mmky_wi_dwr, positive = F),
  pos.neg(dat = mmky_wi_dwr, positive = F, p = fs_ext_precip),
  magnitude(dat= mmky_wi_dwr)*40,
  pos.neg(dat = mmky_su_dwr, positive = T),
  pos.neg(dat = mmky_su_dwr, positive = T, p = fs_ext_precip),
  pos.neg(dat = mmky_su_dwr, positive = F),
  pos.neg(dat = mmky_su_dwr, positive = F, p = fs_ext_precip),
  magnitude(dat= mmky_su_dwr)*40
   ),               
             byrow=T) %>% as.data.frame()
mat
rownames(mat)= c("yearly dwr", "winter dwr" ,"summer dwr")
xtable::xtable(mat, digits=0)

#comparing different time periods
rowname_table=c("spring", "summer", "autumn", "winter")

mat_short = matrix(nrow= length(rowname_table), ncol=2, data = 
               c(
                 magnitude(mmky_spring_short)*NROW(spring_short),
                 magnitude(mmky_summer_short)*NROW(spring_short),
                 magnitude(mmky_autumn_short)*NROW(spring_short),
                 magnitude(mmky_winter_short)*NROW(spring_short)
 ),               
             byrow=T) %>% as.data.frame()

mat_long = matrix(nrow= length(rowname_table), ncol=2, data = 
               c(
                 magnitude(mmky_spring)*NROW(spring),
                 magnitude(mmky_summer)*NROW(spring),
                 magnitude(mmky_autumn)*NROW(spring),
                 magnitude(mmky_winter)*NROW(spring)
 ),               
             byrow=T) %>% as.data.frame()
  
  mat_short = matrix(nrow= length(rowname_table), ncol=2, data = 
               c(
                 magnitude(mmky_spring_short),
                 magnitude(mmky_summer_short),
                 magnitude(mmky_autumn_short),
                 magnitude(mmky_winter_short)
 ),               
             byrow=T) %>% as.data.frame()

mat_long = matrix(nrow= length(rowname_table), ncol=2, data = 
               c(
                 magnitude(mmky_spring),
                 magnitude(mmky_summer),
                 magnitude(mmky_autumn),
                 magnitude(mmky_winter)
 ),               
             byrow=T) %>% as.data.frame()
  
                 
mat = cbind(mat_short,mat_long)
rownames(mat)= rowname_table
xtable::xtable(mat, digits=2)
fs_wi_mn_t

#seasonal temp

winter = seasonal_trends(lb_season=12, ub_season=2, dat = mt_mn_temp, value="temp_m", xtable = T, px=0.03)
spring = seasonal_trends(lb_season=12, ub_season=2, dat = mt_mn_temp, value="temp_m", xtable = T, px=0.03)
summer = seasonal_trends(lb_season=12, ub_season=2, dat = mt_mn_temp, value="temp_m", xtable = T, px=0.03)
autumn = seasonal_trends(lb_season=12, ub_season=2, dat = mt_mn_temp, value="temp_m", xtable = T, px=0.03)
annual = seasonal_trends(lb_season=1, ub_season=12, dat = mt_mn_temp, value="temp_m", xtable = T, px=0.03)

mat = rbind(winter, spring, summer, autumn, annual) %>% 
  as.data.frame() 
rownames(mat) = c("winter","spring","summer","autumn","annual")
mat[,5:6]*40
  
xtable::xtable(mat)

#seasonal precip
#precip sum

winter = seasonal_trends(lb_season=12, ub_season=2, dat = mt_sm_p, value="month_sum", xtable = T, px=0.03,funx="sum", ref=T)
spring = seasonal_trends(lb_season=3, ub_season=5, dat = mt_sm_p, value="month_sum", xtable = T, px=0.03,funx="sum", ref=T)
summer = seasonal_trends(lb_season=6, ub_season=8, dat = mt_sm_p, value="month_sum", xtable = T, px=0.03,funx="sum", ref=T)
autumn = seasonal_trends(lb_season=9, ub_season=11, dat = mt_sm_p, value="month_sum", xtable = T, px=0.03,funx="sum", ref=T)
annual = seasonal_trends(lb_season=1, ub_season=12, dat = mt_sm_p, value="month_sum", xtable = T, px=0.03,funx="sum", ref=T)

mat_sum = c(winter[,c(1,3)], spring[,c(1,3)], summer[,c(1,3)], autumn[,c(1,3)], annual[,c(1,3)])

mat_sum_range = c(winter[,5:6], spring[,5:6], summer[,5:6], autumn[,5:6], annual[,5:6]) 

#dwr

winter = seasonal_trends(lb_season=12, ub_season=2, dat = mt_dwr, value="days_no_rain", xtable = T, px=0.03,funx="sum", ref=F)
spring = seasonal_trends(lb_season=3, ub_season=5, dat = mt_dwr, value="days_no_rain", xtable = T, px=0.03,funx="sum", ref=F)
summer = seasonal_trends(lb_season=6, ub_season=8, dat = mt_dwr, value="days_no_rain", xtable = T, px=0.03,funx="sum", ref=F)
autumn = seasonal_trends(lb_season=9, ub_season=11, dat = mt_dwr, value="days_no_rain", xtable = T, px=0.03,funx="sum", ref=F)
annual = seasonal_trends(lb_season=1, ub_season=12, dat = mt_dwr, value="days_no_rain", xtable = T, px=0.03,funx="sum", ref=F)

mat_dwr = c(winter[,c(1,3)], spring[,c(1,3)], summer[,c(1,3)], autumn[,c(1,3)], annual[,c(1,3)]) 

mat_dwr_range = c(winter[,5:6], spring[,5:6], summer[,5:6], autumn[,5:6], annual[,5:6]) 

#extreme precip 
winter = seasonal_trends(lb_season=12, ub_season=2, dat = mt_ext_p, value="severe_events", xtable = T, px=0.03,funx="max", ref=F)
spring = seasonal_trends(lb_season=3, ub_season=5, dat = mt_ext_p, value="severe_events", xtable = T, px=0.03,funx="max", ref=F)
summer = seasonal_trends(lb_season=6, ub_season=8, dat = mt_ext_p, value="severe_events", xtable = T, px=0.03,funx="max", ref=F)
autumn = seasonal_trends(lb_season=9, ub_season=11, dat = mt_ext_p, value="severe_events", xtable = T, px=0.03,funx="max", ref=F)
annual = seasonal_trends(lb_season=1, ub_season=12, dat = mt_ext_p, value="severe_events", xtable = T, px=0.03,funx="max", ref=F)

mat_ext_p = c(winter[,c(1,3)], spring[,c(1,3)], summer[,c(1,3)], autumn[,c(1,3)], annual[,c(1,3)]) 

mat_ext_p_range = c(winter[,5:6], spring[,5:6], summer[,5:6], autumn[,5:6], annual[,5:6]) 

mat= rbind(mat_sum, mat_dwr, mat_ext_p)

rownames(mat) = c("precipitation sum","days without rain","72h precipitation")
colnames(mat) = rep(c("+","-"),5)

#ranges

mat= rbind(mat_sum_range, mat_dwr_range*40, mat_ext_p_range*40)

rownames(mat) = c("precipitation sum","days without rain","72h precipitation")
xtable::xtable(mat, digits=0)
