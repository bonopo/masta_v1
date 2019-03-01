#xtable script for LaTeX tables
#generel low flow trends
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
                 magnitude(dat = mmky_ms7_min, reference = gauges$mn_q),
                 pos.neg(p=NULL, dat = mmky_ms7_date, positive = T),
                 pos.neg(p=fs_ms7_date, dat = mmky_ms7_date, positive = T),
                 pos.neg(p=NULL, dat = mmky_ms7_date, positive = F),
                 pos.neg(p=fs_ms7_date, dat = mmky_ms7_date, positive = F),
                 round(fs_ms7_date,2),
                 magnitude(dat = mmky_ms7_date)
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
                 magnitude(dat = mmky_yearly_mn_t),
                 pos.neg(p=NULL, dat = mmky_wi_mn_t, positive = T),
                 pos.neg(p=fs_wi_mn_t, dat = mmky_wi_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_wi_mn_t, positive = F),
                 pos.neg(p=fs_wi_mn_t, dat = mmky_wi_mn_t, positive =F),
                 round(fs_wi_mn_t,2),
                 magnitude(dat = mmky_wi_mn_t),
                 pos.neg(p=NULL, dat = mmky_sp_mn_t, positive = T),
                 pos.neg(p=fs_sp_mn_t, dat = mmky_sp_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_sp_mn_t, positive = F),
                 pos.neg(p=fs_sp_mn_t, dat = mmky_sp_mn_t, positive = F),
                 round(fs_sp_mn_t,2),
                 magnitude(dat = mmky_sp_mn_t),
                 pos.neg(p=NULL, dat = mmky_su_mn_t, positive = T),
                 pos.neg(p=fs_su_mn_t, dat = mmky_su_mn_t, positive = T),
                 pos.neg(p=NULL, dat = mmky_su_mn_t, positive = F),
                 pos.neg(p=fs_su_mn_t, dat = mmky_su_mn_t, positive = F),
                 round(fs_su_mn_t,2),
                 magnitude(dat = mmky_su_mn_t),
                 pos.neg(p=NULL, dat = mmky_wi_days_below_0, positive = T),
                 pos.neg(p=fs_wd_0, dat = mmky_wi_days_below_0, positive = T),
                 pos.neg(p=NULL, dat = mmky_wi_days_below_0, positive = F),
                 pos.neg(p=fs_wd_0, dat = mmky_wi_days_below_0, positive = F),
                 round(fs_wd_0,2),
                 magnitude(dat = mmky_wi_days_below_0)
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



