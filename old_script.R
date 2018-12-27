
# old drought characterisation -----------------------------------------------------------

year = year(date_seq) %>% list()
mt_mn_q$year <- unlist(rep(year, times = catch_n))
mnq30 <- aggregate(mt_mn_q_wide, by= year, FUN= min, by.column=T)

mnq30_long <- gather(mq30, key=gauge, value= mq30, -date ) %>% as.tbl()

#mnq30 date

mnq30_date <- mnq30_long %>% 
  group_by(year(date), gauge) %>% 
  summarise(date[which.min(mq30)]) %>% 
  ungroup()

colnames(mnq30_date) <- c("year", "gauge", "date_mnq30")

#measure of distance to june to overcome problem 12 - 1####
 

# old SCI calculation -----------------------------------------------------
# SSI calculation ---------------------------------------------------------


# mean discharge extraction for every gauge 
# 
# 
# for (i in 1){
#   q_by_month <- month_ext(monthx = i, datax = mt_mn_q)
#   assign(paste0("q_",str_to_lower(month.abb[i])), q_by_month)
# }  
#normalising and standardizing data to follow cumulative normal distribution with mean 0 and sd = 1

  gauges_ssi <- data.frame()
  ssi_entire <- data.frame()

  for (i in 1:12){
     data <- month_ext(monthx = i, datax = mt_mn_q)
     for (g in 1:ncol(data)){
     gauge <- data[,g]
     stan <- (gauge-mean(gauge))/sd(gauge)
     gauges_ssi[(1:length(gauge)),g] <- stan
     }
    gauges_ssi$yr_mt <- seq.Date(from= ymd(paste("1970",i, "15", sep="-")), to = ymd(paste("2009",i,"15", sep="-")), by="year")  
    ssi_entire <- rbind(ssi_entire,gauges_ssi)
  } 
  
  
ssi_sorted <- ssi_entire[order(as.Date(ssi_entire$yr_mt)),]
ssi_sorted %<>% dplyr::select(-(yr_mt))
ssi_sorted %>% unlist() %>% is.infinite() %>% any()
remove(ssi_entire,data)

# the standartization (p.42p in satistical methods in the atmospheric sciences) removes the interannual variation forms a distribution with mean 0 and sd 1 but it does not remove the skewness (see hist(janssi[,6]) it is still skewed to the right
#if not n+1 than that would mean that the probability to measure an event higher than the highest is 0

mean(ssi_sorted$V100)



# decompose time series into trend and seasonal part ----------------------

ts = ts(mt_mn_q_wide, start=c(1970,1), end=c(2009,12), deltat=1/12)
ssi_dec <- decompose(ssi_ts[,1])
plot(ssi_dec)
res=decompose(ts)
plot(res)
res$trend[,1] %>% plot()


#compare to mmky#
resi = list()
for (i in 1:catch_n){
resi[[i]] = mmky(c(res$trend[,i]))


}
 pp = do.call("rbind",resi) %>% as.data.frame()
 plot(pp$`Sen's slope`)



# Calculatin SPEI with different distributions ----------------------------

# with Generalized Logistic Distribution
spei_gl <- sci_calc(distx = "genlog",datax = mt_sm_p$month_sum, gaugex =mt_sm_p$gauge,  agg_n = 6)

png("spi_comparison.png")
plot(y=temp[[338]], x= date_seq,  type="l", ylab="SPI-6", xlab="")
lines(y=spei_gl[[338]],x= date_seq, col=3)
lines(y=spi_6$`338`, x= date_seq,col=2)
legend("bottomright", col=c(1,3,2), lty=c(1,1,1), c("gamma", "gen. logistic", "nonparametric"), bty="n")
dev.off()
#with Generalized extreme value distribution
#spei_gev <- sci_calc(distx = "gev") #doesn't work

#with gamma distribution
spei_gam <- sci_calc(distx="gamma") #bad result three parameter distribution used for arid regions because it can handle negative values better vicente-serrano et al 2010
#converting SPEI objects into vectors

# spei_gv_v <- spei_vec(spei_gev)
# spei_gl_v <- spei_vec(spei_gl)

# m1 <- matrix(spei_gl_v, nrow = 480, byrow =F)
# spi_df <- as.data.frame(m1) 

# Distribution comparison SPEI -----------------------------------

#merging data

spei <- cbind(spei_6, spei_gv_v, spei_gl_v) %>%
  as.data.frame() %>%
  as.tbl() %>%
  mutate(date= mt_mn_temp$yr_mt) %>%
  mutate(gauge= mt_mn_temp$gauge )

spei_long <- gather(spei, distr, value, -date, -gauge)
  spei_long %>%
    filter(date < 1980 & gauge == 1) %>%
ggplot()+
  geom_line(aes(x= date, y=value,  color=distr), stat="identity")

#L-moments diagrams

#Kolmogorow-Smirnow-Test
  ks.test(x=spei_data$p_pet, y=spei_vec(spei_6, spei = TRUE), alternative = "t" )
  ks.test(x=spei_data$p_pet, y=spei$spei_gv_v, alternative = "t" )
  ks.test(x=spei_data$p_pet, y=spei$spei_gl_v, alternative = "t" )
  # similar d values the lower the better

  mt_mn_temp$gauge

# SSI Calculation using parametric solution -------------------------------

# ssi_p3 <- sci_calc(datax = mt_sum_q$q_sum, gaugex = mt_sum_q$gauge, distx = "pe3", agg_n = 1, p0x = F ) 
# ssi_wb <- sci_calc(datax = mt_sum_q$q_sum, gaugex = mt_sum_q$gauge, distx = "weibull", agg_n = 1, p0x = F ) 
# ssi_gb <- sci_calc(datax = mt_sum_q$q_sum, gaugex = mt_sum_q$gauge, distx = "gumbel", agg_n = 1, p0x = F ) 
# ssi_ln <- sci_calc(datax = mt_sum_q$q_sum, gaugex = mt_sum_q$gauge, distx = "lnorm", agg_n = 1, p0x = F ) 
# mt_sum_q$ssi_p3 <- spei_vec(ssi_p3, gaugex = mt_sum_q$gauge)
# mt_sum_q$ssi_wb <- spei_vec(ssi_wb, gaugex = mt_sum_q$gauge)
# mt_sum_q$ssi_gb <- spei_vec(ssi_gb, gaugex = mt_sum_q$gauge)
# mt_sum_q$ssi_ln <- spei_vec(ssi_ln, gaugex = mt_sum_q$gauge)


#NA because of fitting errors
#it seems that it can not fit destinct values for certain distributions
# calculating different probability distribution for every month and station as of recommended by shukla and wood 2008 and vicene-serrano et al 2012
  

#parameter estimation loglikelihood
#one sided ks test
#pvalue > 0.05 means that the zero hypothesis can not be rejected
# 0 hypothesis is that they are not from the same distribution
#gev

# gev <- fgev(bbb)
# pa <- gev$estimate %>% 
#   as.list()
# pa <- fitdist(bbb, distr = "gev", discrete = FALSE, start = pa)$estimate #log likelihood = default
# ks.test(bbb, "pgev",pa )
# 
# #pIII
# PE3<-parpe3(lmom.ub(bbb)) 
# ks.test(bbb, "cdfpe3", PE3)
# 
# #log normal
# pa <- fitdist(bbb, distr = "lnorm", discrete = FALSE)$estimate #log likelihood = default
# ks.test(bbb, "plnorm",pa )
# e_bbb <- ecdf(bbb)
# 
# #weibull
# pa <- fitdist(bbb, "weibull", discrete = FALSE)$estimate  #log likelihood = default
# # bbb_sort <- sort(bbb, decreasing = TRUE)
# # cumsum(bbb_sort)
# # n <- length(bbb)
# # Femp <- 1:n/(n+1)
# 
# ks.test(Femp, "pweibull",pa)
# 
# #log logistic
# fnLLLL = function(vParams, vData) {
#   # uses the density function of the log-logistic function from FAdist
#   return(-sum(log(dllog(vData, shape = vParams[1], scale = vParams[2]))))
# }
# # optimize it
# res <- optim(c(2, 3), fnLLLL, vData = bbb)$par 
# pa <- list()
# pa$shape <- res[1]
# pa$scale <- res[2]
# ks.test(bbb, "pllog",shape = pa$shape, scale= pa$scale)
# 
# #generalized Pareto
# pa <- pargpa(lmom.ub(bbb))
# ks.test(bbb, "cdfgpa", pa)
# 
# 
# 
# 
# 
# 
# #ks test compares overall shape of distribution not specifically central tendency, dispersion or other parameters
# 
# 
# sci_calc <- function(datax = spei_data$p_pet, gaugex=spei_data$gauge, distx="gev", agg_n=6, p0x=F){
#  output <- as.list(NA)
# for(i in unique(gaugex)){
# data <- datax[gaugex==i]
# params_dist <- fitSCI(data, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
# #inital values for parameters calculated with L-moments and then optimization with maximum likelihood
# #if there are a lot of zeros in the time series it is recommended to use p0=TRUE
# spi_temp <- transformSCI(data, first.mon = 1, obj = params_dist)
# output[[i]] <- spi_temp
# }  
#  return(output)
#   }


  
  
  
  



 # (OLD) SPEI vs. SPI comparison with regression (OLD)-------------------------------------------------

lm_spei_ssi <- spi_spei_reg(pred = "spei_v2") #[1]intercept, [2]slope [3] r²
lm_spi_ssi <- spi_spei_reg(pred="spi_v2")

sci_4_i <- sci_reg(sci_n = 4, interaction = T)


stat <- c()
for (i in 1:338){
  stat[i] <- sci_4_i[[i]]$adj.r.squared
}

pdf("./plots/sci_4_i_r2.pdf") #i interaction, ni no interaction
plot(stat, t="p")
dev.off()

#plots
# pdf("./plots/spi-ssi_regression.pdf")
# par(mfrow=c(1,3))
# boxplot(lm_spi_ssi[[1]], ylab="intercept", xlab="spi-n" ) #intercept
# boxplot(lm_spi_ssi[[2]], ylab="slope", xlab="spi-n") #slope
# boxplot(lm_spi_ssi[[3]], ylab="r²", xlab="spi-n") #r²
# dev.off()



# mann- kendall test ------------------------------------------------------


ken_spei <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), data_source =  "spei_", sci = TRUE)
ken_spi <- ken_trend(agg_mn= c(1,2,3,6,9,12,24), sci = TRUE, data_source =  "spi_")
ken_ssi <- ken_trend(data_source =  "ssi_1", sci = FALSE )
res=sapply(ms7_min[,1:338], FUN=mkttest)
res %>% t() %>% head()
bb_ms7_min %>% head()
plot(x = res[6,], y=unlist(bb_ms7_min[,4]))


# quantil trend ####


quant_trend_1 <- qua_trend(quantil = 0.1, data_source = "q_long")
quant_trend_05 <- qua_trend(quantil = 0.05, data_source = "q_long") # wie schweizer defnition Q347

pdf("./plots/mk_quant.pdf")
plot(quant_trend_05$tau, ylab="tau", xlab="catchments", ylim=c(-0.65, .45))
points(quant_trend_1$tau, col=2)
legend("bottomleft", pch=c(1,1), col=c(1,2), c("quantil = .05", "quantil = .1"), bty="n")
abline(h=0, lty=2, col=4)
dev.off()


#seasonal mk test ####
ken_summer_min_q = ken_trend(data_source = "summer_min_q", sci=FALSE)
gauges$ken_summer_min_q =ken_summer_min_q[,1]

gauges$summer_ave_q = ken_trend(data_source = "summer_ave_q", sci=FALSE)[,1]
gauges$summer_sum_p = ken_trend(data_source = "summer_sum_p", sci=FALSE)[,1]
gauges$summer_q_q10 = ken_trend(data_source = "summer_q_q10", sci=FALSE)[,1]




# drought attribution: SPI or SPEI? with linear regression ####


spi_ssi = spi_spei_reg(sci = "spi_") #1 =intercept 2= slope 3 = rsq
spei_ssi = spi_spei_reg(sci = "spei_") 
best_spi = c()
value_spi = c()
best_spei = c()
value_spei = c()

plot(spi_ssi[[3]][,3])
points(spi_ssi_v2[[3]][,3], col=2)

for(r in 1:catch_n){
 best_spi[r] = spi_ssi[[3]][r,] %>% which.max()
 value_spi[r] = spi_ssi[[3]][r,] %>% max()}
gauges$reg_spi_n = best_spi
gauges$reg_spi_n = value_spi
for(r in 1:catch_n){
 best_spei[r] = spei_ssi[[3]][r,] %>% which.max()
 value_spei[r] = spei_ssi[[3]][r,] %>% max()}

gauges$reg_spei_n = best_spei
gauges$reg_spei_n =value_spei

# pdf("./plots/spi_spei_reg.pdf")
# plot(best_spi - best_spei)
# dev.off()
# 
# pdf("./plots/spi_spei_reg_rsq.pdf")
# plot(value_spi, ylab="best r²")
# points(value_spei, col=2)
# legend("bottomleft", col=c(1,2), pch=c(1,1), c("spi", "spei"), bty="n")
# dev.off()
# 
# pdf("./plots/spi_spei_opt_agg_n.pdf")
# plot(best_spi, ylab="SPI-/SPEI-n with lowest r²")
# points(x=which(best_spei != best_spi), y=best_spei[best_spei != best_spi], col=2)
# legend("topleft", c("spi", "spei(only if diff. to spi)"), col=c(1,2), pch=c(1,1), bty="n")
# dev.off()



# kendall rank correlation -----------------------------------------------------

mk_spi_tau <- list()
mk_spi_S <- list()
mk_spi_D <- list()
mk_spi_p <- list()
for (i in 1:ncol(spi_2)){
 mk_spi_tau[[i]] <-  Kendall(spi_2[,i], ssi_1[,i])$tau[1] 
 mk_spi_S[[i]] <- Kendall(spi_2[,i], ssi_1[,i])$S[1] 
mk_spi_D[[i]] <- Kendall(spi_2[,i], ssi_1[,i])$D[1] 
mk_spi_p[[i]] <- Kendall(spi_2[,i], ssi_1[,i])$sl[1] 
}

Kendall(spi_1[,44], ssi_1[,44])
#tau ist S/D
#S anzahl an positiven - negativen trends
#D value theoretisch mögliche maximale anzahl an trends
plot(x =spi_2[,1] , y=ssi_1[,1])
#offensichtliche aussage: desto höher der spi, desto höher der spei 


#seasonal 80th % analysis approach####
#NOT usable see notations in script why shouldn't be used rather use seasonal_80th function 
sum_q = drought_q %>% 
  as.tbl %>% 
  mutate(dr_start = ymd(dr_start), dr_end=ymd(dr_end), catchment= as.integer(catchment)) %>%   group_by(catchment, year = as.integer(year(dr_start))) %>% 
  summarise(
    n_events = n(),
    sm_length = as.numeric(sum(ymd(dr_end)-ymd(dr_start))),#not usable since there are droughts that go over several years
    tot_defi = as.numeric(round(sum(def_vol*(dr_end-dr_start)),0)),#not usable because drought deficit gets allocated to one year even if it goes over several years
    mn_defi = round(mean(def_vol)),
    mn_length= round(sm_length/n_events,0)
  ) %>% 
  mutate(mn_length = as.integer(mn_length))
#deficit vol in m³ increase per day of drought

tot_defi_catch = sum_q %>% 
  group_by(catchment) %>% 
  summarise(sd_tot_defi = sd(tot_defi)) %>% 
  mutate(hydrogeo = gauges$hydrogeo_simple)

int = which.max(tot_defi_catch$sd_tot_defi) # extreme

pdf("./plots/4_choice/geo_sd_tot_defi.pdf")
boxplot(log10(sd_tot_defi) ~ hydrogeo, data=tot_defi_catch[-int,], ylab="log10 sd total deficit")
dev.off()
tot_defi_q= sum_q %>% 
  ungroup() %>% 
  mutate(stan_defi = (tot_defi - mean(tot_defi))/sd(tot_defi)) %>% #has to be standadized see laaha et al 2015
  dplyr::select(catchment, stan_defi, year) %>% 
  spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
  dplyr::select(-year)%>% 
  as.data.frame()


mmky_tot_defi_q$sen_slope[128] %>% which.max()

ggplot()+
  geom_line(data=sum_q %>% filter(catchment == 128), aes(x= year, y= tot_defi))

# mn_defi_q= sum_q %>% 
#   ungroup() %>% 
#   mutate(stan_defi = (mn_defi - mean(mn_defi))/sd(mn_defi)) %>% #has to be standadized see laaha et al 2015
#   dplyr::select(catchment, stan_defi, year) %>% 
#   spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
#   dplyr::select(-year)%>% 
#   as.data.frame()

# mn_length_q= sum_q %>% 
#   ungroup() %>% 
#   dplyr::select(catchment, mn_length, year) %>% 
#   spread(., key=(catchment), value=(mn_length), fill = 0)%>% 
#   dplyr::select(-year)%>% 
#   as.data.frame()

sm_length_q= sum_q %>% 
  ungroup() %>% 
  dplyr::select(catchment, sm_length, year) %>% 
  spread(., key=(catchment), value=(sm_length), fill = 0)%>% 
  dplyr::select(-year)%>% 
  as.data.frame()


sum_p= drought_p %>% 
  as.tbl %>% 
  mutate(dr_start = ymd(dr_start), dr_end=ymd(dr_end), catchment= as.integer(catchment)) %>%   group_by(catchment, year = as.integer(year(dr_start))) %>% 
  summarise(n_events = n(), sm_length = as.numeric(sum(dr_end-dr_start)), tot_defi = as.numeric(round(sum(def_vol*(dr_end-dr_start)),0)),  mn_defi = as.numeric(round(tot_defi/n_events,0)),mn_length= round(sm_length/n_events,0)) %>% 
  mutate(mn_length = as.integer(mn_length))



tot_defi_p= sum_p %>% 
  ungroup() %>% 
  mutate(stan_defi = (tot_defi - mean(tot_defi))/sd(tot_defi)) %>% 
  dplyr::select(catchment, stan_defi, year) %>% 
  spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
  dplyr::select(-year) %>% 
  as.data.frame()

# mn_defi_p= sum_p %>% 
#   ungroup() %>% 
#   mutate(stan_defi = (mn_defi - mean(mn_defi))/sd(mn_defi)) %>% #has to be standadized see laaha et al 2015
#   dplyr::select(catchment, stan_defi, year) %>% 
#   spread(., key=(catchment), value=(stan_defi), fill = 0) %>% 
#   dplyr::select(-year)%>% 
#   as.data.frame()

# mn_length_p= sum_p %>% 
#   ungroup() %>% 
#   dplyr::select(catchment, mn_length, year) %>% 
#   spread(., key=(catchment), value=(mn_length), fill = 0)%>% 
#   dplyr::select(-year)%>% 
#   as.data.frame()

sm_length_p= sum_p %>% 
  ungroup() %>% 
  dplyr::select(catchment, sm_length, year) %>% 
  spread(., key=(catchment), value=(sm_length), fill = 0)%>% 
  dplyr::select(-year)%>% 
  as.data.frame()




#seasonal 80th % (calculation sequential USE PARALLEL)####
#takes 10 min to calculate!!!!!
stime = Sys.time()
mat_def = matrix(ncol=catch_n, nrow=12)
mat_n = matrix(ncol=catch_n, nrow = 12)
months = c()
for( m in 1:12){


for (c in 1:10){  #change to catch_n
  temp1 = drought_q %>% 
    filter(catchment == c)
def_mean = NULL
n_mean= NULL
 for (e in 1:max(temp1$event_no)){
   #retrieving the month of the drought. drought that go over more than one month get allocated to each of the effected months
   if((year(temp1$dr_start[e])+1) == year(temp1$dr_end[e]) ){
    months = c(seq(from=month(temp1$dr_start[e]), to=12, by=1), seq(from=1, to   =month(temp1$dr_end[e]), by=1))}
   if(year(temp1$dr_start[e]) == year(temp1$dr_end[e])){
      months = seq(from = month(temp1$dr_start[e]) , to= month(temp1$dr_end[e]), by=1)} 
   if((year(temp1$dr_end[e]) - year(temp1$dr_start[e])) > 1){
     months = 1:12
   }
   
   #retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought. if the drought is longer than one month the cumulative sum of the deficit volume gets devided by number of month (including partial months, meaning a drought going from mid dec. to end february: every month would get allocated a 1/3 of the total cumulative drought. 33% percent because it is three month: dec., jan. and feb.)
   
   dr_len = as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))
   
 if(m %in% months){
   def_mean = rbind(def_mean,temp1$def_vol[e]*(dr_len/length(months))) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them
 }else{
   next
 }
 }  


mat_def[m,c] = round(mean(def_mean),0)
mat_n[m,c] = length(def_mean)
if(c %% 20 == 0) cat(round((c+((m-1)*catch_n))/(catch_n*12),2) * 100, "%", "\n")
  
 
}
cat(month.name[m], "just finished", "\n")
}
Sys.time()-stime

#seasonal 80th (parallel version1)####

par_seas_80th = function(data= drought_q , catchment_max=catch_n){
no_cores=detectCores()
cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()
def_catch = c()
mean_def=c()
mean_n = c()
sm_length=c()

pb <- txtProgressBar(max = catchment_max, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

res <- foreach::foreach(c = 1:catchment_max, .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ ###cbind
 #sub_80th =  function(c) 
    temp1 = data %>%
    filter(catchment == c)
for (m in 1:12){
  def_catch=NULL
  days_catch=NULL
for (e in 1:max(temp1$event_no)){
  days_dr = NULL
  dr_len = NULL
   if((year(temp1$dr_start[e])+1) == year(temp1$dr_end[e]) ){
    months = c(seq(from=month(temp1$dr_start[e]), to=12, by=1), seq(from=1, to   =month(temp1$dr_end[e]), by=1))}
   if(year(temp1$dr_start[e]) == year(temp1$dr_end[e])){
      months = seq(from = month(temp1$dr_start[e]) , to= month(temp1$dr_end[e]), by=1)}
   if((year(temp1$dr_end[e]) - year(temp1$dr_start[e])) > 1){
     months = 1:12
   }

#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought. if the drought is longer than one month the cumulative sum of the deficit volume gets devided by number of month (including partial months, meaning a drought going from mid dec. to end february: every month would get allocated a 1/3 of the total cumulative drought. 33% percent because it is three month: dec., jan. and feb.)
   
  
 if(m %in% months){
   dr_len = as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))
   def_catch = rbind(def_catch,temp1$def_vol[e]*(dr_len/length(months))) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them
   if(length(months) == (12)){
     days_dr = as.numeric((year(temp1$dr_end[e]) - year(temp1$dr_start[e]))*days_in_month(m))
   }
      if(m > month(temp1$dr_start[e]) & m < month(temp1$dr_end[e])){
     days_dr = days_in_month(m)}
   if(m == month(temp1$dr_start[e]) & m == month(temp1$dr_end[e]) ){
     days_dr = as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))}
   if(m == month(temp1$dr_start[e]) & is.null(days_dr)){
     days_dr = as.numeric(days_in_month(m) - day(ymd(temp1$dr_start[e])))}
   if(m == month(temp1$dr_end[e]) & is.null(days_dr)){
     days_dr = as.numeric(day(ymd(temp1$dr_end[e])))
   }
   
   days_catch = rbind(days_catch, days_dr)
   
 }else{
   next
 }
 }  

mean_def[m] = round(sum(def_catch),0)
mean_n[m] = length(def_catch)
sm_length[m] = sum(days_catch) #total sum of days


}

    #return(mean_def) 
cbind(mean_def, mean_n, sm_length)
# temp2
  }
close(pb)
stopCluster(cl)

mean_def_list <- lapply(res, function(x) x[,1])
mean_def_df = do.call( "cbind",mean_def_list) %>% as.data.frame() %>% set_colnames(1:catchment_max)

mean_n_list <- lapply(res, function(x) x[,2])
mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catchment_max)

sm_days_list <- lapply(res, function(x) x[,3])
sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catchment_max)

return(list(mean_def_df,mean_n_df, sm_days_df))
# return(res)


}

#seasonal 80th with monthly sm days calculation####
seasonal_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th =  function(i) {
  
  sum_def=c()
  mean_n = c()
  sm_length = c()
  
    temp1 = data %>%
    filter(catchment == i)
    mat = matrix(nrow=length(year_ta), ncol=12)
for (m in 1:12){
  def_catch=NULL
  days_catch=NULL
for (e in 1:max(temp1$event_no)){
  days_dr = NULL
   months= NULL
   
   if((year(temp1$dr_start[e])+1) == year(temp1$dr_end[e]) ){
    months = c(seq(from=month(temp1$dr_start[e]), to=12, by=1), seq(from=1, to   =month(temp1$dr_end[e]), by=1))}
   if(year(temp1$dr_start[e]) == year(temp1$dr_end[e])){
      months = seq(from = month(temp1$dr_start[e]) , to= month(temp1$dr_end[e]), by=1)}
   if((year(temp1$dr_end[e]) - year(temp1$dr_start[e])) > 1){
     months = 1:12
   }
 
#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought. if the drought is longer than one month the cumulative sum of the deficit volume gets devided by number of month (including partial months, meaning a drought going from mid dec. to end february: every month would get allocated a 1/3 of the total cumulative drought. 33% percent because it is three month: dec., jan. and feb.)
   
  
 if(m %in% months){

   #calculating number of days that are affected by drought
  if(length(months) == (12)){
     days_dr = as.numeric((year(temp1$dr_end[e]) - year(temp1$dr_start[e]))*days_in_month(m))}
  if(length(months) > 2 & is.null(days_dr) & m %in% months[c(-1,-length(months))]){
     days_dr = days_in_month(m)}
  if(m == month(temp1$dr_start[e]) & m == month(temp1$dr_end[e])& is.null(days_dr) ){
     days_dr = as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))}
  if(m == month(temp1$dr_start[e]) & is.null(days_dr)){
     days_dr = as.numeric(days_in_month(m) - day(ymd(temp1$dr_start[e])))}
  if(m == month(temp1$dr_end[e]) & is.null(days_dr)){
     days_dr = as.numeric(day(ymd(temp1$dr_end[e])))}
  if(year(ymd(temp1$dr_start[e])) < year(ymd(temp1$dr_end[e])) & m < tail(months,1) & is.null(days_dr)){
     days_dr = days_in_month(m) %>% as.numeric()
        }
   # if(m == 1)  cat(days_dr,e, "\n")
  
  
     
   days_catch = rbind(days_catch, days_dr) #days per catchment effected by drought #
   def_catch = rbind(def_catch,temp1$def_vol[e]*days_dr) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them proportional to the amount of days effected
  
 
   # if(is.null(days_dr)) {
   #  # stop(e, "row", i, "catchment")
   #   cat(e, "row", i, "catchment", m, "\n")}
 }else{
   next
 }
}
  


sum_def[m] = round(sum(def_catch),0) #sum deficit per catchment per month
mean_n[m] = length(def_catch) # mean number of events per month per catchment
sm_length[m] = sum(days_catch) #total sum of days effected in every month per catchment #


}
   return(cbind(sum_def, mean_n,sm_length)) 

}


cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:catch_n, .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ 
   sub_80th(i=c)
                        }
close(pb)
stopCluster(cl)
#save(res,file="./output/seasonal_q.Rdata")

sum_def_list <- lapply(res, function(x) x[,1])
sum_def_df = do.call( "cbind",sum_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
mean_n_list <- lapply(res, function(x) x[,2])
mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
sm_days_list <- lapply(res, function(x) x[,3])
sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catch_n)

return(list(sum_def_df,mean_n_df,sm_days_df))
}

#seasonal with monthly and yearly calculation ####
seasonal_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th =  function(i) {
  
  sum_def=c()
  mean_n = c()
  sm_length = c()
  
    temp1 = data %>%
    filter(catchment == i)
    mat = matrix(nrow=length(year_ta), ncol=12)
for (m in 1:12){
  def_catch=NULL
  days_catch=NULL
for (e in 1:max(temp1$event_no)){
  days_dr = NULL
   months= NULL
   
   if((year(temp1$dr_start[e])+1) == year(temp1$dr_end[e]) ){
    months = c(seq(from=month(temp1$dr_start[e]), to=12, by=1), seq(from=1, to   =month(temp1$dr_end[e]), by=1))}
   if(year(temp1$dr_start[e]) == year(temp1$dr_end[e])){
      months = seq(from = month(temp1$dr_start[e]) , to= month(temp1$dr_end[e]), by=1)}
   if((year(temp1$dr_end[e]) - year(temp1$dr_start[e])) > 1){
     months = 1:12
   }
  mt_yr = seq.Date(from=ymd(temp1$dr_start[e]), to= ymd(temp1$dr_end[e]), by="month")
   if(month(ymd(temp1$dr_start[e])) !=  month(ymd(temp1$dr_end[e])) | length(months) > 1){
     mt_yr = c(mt_yr, ymd(temp1$dr_end[e]))
   }
  year_y = year(mt_yr)
  month_x = month(mt_yr)

#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought. if the drought is longer than one month the cumulative sum of the deficit volume gets devided by number of month (including partial months, meaning a drought going from mid dec. to end february: every month would get allocated a 1/3 of the total cumulative drought. 33% percent because it is three month: dec., jan. and feb.)
   
  
 if(m %in% months){
   years_of_drought =  pmatch(year_y,year_ta,duplicates.ok = F)
   #calculating number of days that are affected by drought
     if(length(months) == (12)){
     #days_dr = as.numeric((year(temp1$dr_end[e]) - year(temp1$dr_start[e]))*days_in_month(m))
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = days_in_month(m) #
   }
      if(length(months) > 2 & is.null(days_dr) & m %in% months[c(-1,-length(months))]){
     #days_dr = days_in_month(m)
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = days_in_month(m)}#
   if(m == month(temp1$dr_start[e]) & m == month(temp1$dr_end[e])& is.null(days_dr) ){
    # days_dr = as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))}
      mat[years_of_drought[which(!is.na(years_of_drought))], m] =  as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))}
   if(m == month(temp1$dr_start[e]) & is.null(days_dr)){
     #days_dr = as.numeric(days_in_month(m) - day(ymd(temp1$dr_start[e])))}
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = as.numeric(days_in_month(m) - day(ymd(temp1$dr_start[e])))}
   if(m == month(temp1$dr_end[e]) & is.null(days_dr)){
     #days_dr = as.numeric(day(ymd(temp1$dr_end[e])))}
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = as.numeric(day(ymd(temp1$dr_end[e])))}
     
   if(year(ymd(temp1$dr_start[e])) < year(ymd(temp1$dr_end[e])) & m < tail(months,1) & is.null(days_dr)){
     #days_dr = days_in_month(m) %>% as.numeric()
      cat(e, "row", i, "catchment", m, "\n")
      mat[years_of_drought[which(!is.na(years_of_drought))], m] = days_in_month(m)
   }
   # if(m == 1)  cat(days_dr,e, "\n")
  
  
     
   #days_catch = rbind(days_catch, days_dr) #days per catchment effected by drought #
   def_catch = rbind(def_catch,temp1$def_vol[e]*days_dr) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them proportional to the amount of days effected
   print(mat,"\n")#
   #print(temp1[e,], "\n") #
   Sys.sleep(10)#
 
   # if(is.null(days_dr)) {
   #  # stop(e, "row", i, "catchment")
   #   cat(e, "row", i, "catchment", m, "\n")}
 }else{
   next
 }
}
  


sum_def[m] = round(sum(def_catch),0) #sum deficit per catchment per month
mean_n[m] = length(def_catch) # mean number of events per month per catchment
#sm_length[m] = sum(days_catch) #total sum of days effected in every month per catchment #


}
return(list(sum_def, mean_n,mat))
#    return(cbind(sum_def, mean_n,sm_length)) #

}


cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:catch_n, .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ 
   sub_80th(i=c)
                        }
close(pb)
stopCluster(cl)
#save(res,file="./output/seasonal_q.Rdata")

sum_def_list <- lapply(res, function(x) x[,1])
sum_def_df = do.call( "cbind",sum_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
mean_n_list <- lapply(res, function(x) x[,2])
mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
sm_days_list <- lapply(res, function(x) x[,3])
sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catch_n)

return(list(sum_def_df,mean_n_df,sm_days_df))
}



seasonal_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th =  function(i) {
  
  sum_def=c()
  mean_n = c()
  sm_length = c()
  
    temp1 = data %>%
    filter(catchment == i)
    mat = matrix(nrow=length(year_ta), ncol=12)
for (m in 1:12){
  def_catch=NULL
  days_catch=NULL
for (e in 1:max(temp1$event_no)){
  days_dr = NULL
   months= NULL
   
   if((year(temp1$dr_start[e])+1) == year(temp1$dr_end[e]) ){
    months = c(seq(from=month(temp1$dr_start[e]), to=12, by=1), seq(from=1, to   =month(temp1$dr_end[e]), by=1))}
   if(year(temp1$dr_start[e]) == year(temp1$dr_end[e])){
      months = seq(from = month(temp1$dr_start[e]) , to= month(temp1$dr_end[e]), by=1)}
   if((year(temp1$dr_end[e]) - year(temp1$dr_start[e])) > 1){
     months = 1:12
   }
  mt_yr = seq.Date(from=ymd(temp1$dr_start[e]), to= ymd(temp1$dr_end[e]), by="month")
   if(month(ymd(temp1$dr_start[e])) !=  month(ymd(temp1$dr_end[e])) | length(months) > 1){
     mt_yr = c(mt_yr, ymd(temp1$dr_end[e]))
   }
  year_y = year(mt_yr)
  month_x = month(mt_yr)

#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought. if the drought is longer than one month the cumulative sum of the deficit volume gets devided by number of month (including partial months, meaning a drought going from mid dec. to end february: every month would get allocated a 1/3 of the total cumulative drought. 33% percent because it is three month: dec., jan. and feb.)
   
  
 if(m %in% months){
   years_of_drought =  pmatch(year_y,year_ta,duplicates.ok = F)
   #calculating number of days that are affected by drought
     if(length(months) == (12)){
     #days_dr = as.numeric((year(temp1$dr_end[e]) - year(temp1$dr_start[e]))*days_in_month(m))
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = days_in_month(m) #
   }
      if(length(months) > 2 & is.null(days_dr) & m %in% months[c(-1,-length(months))]){
     #days_dr = days_in_month(m)
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = days_in_month(m)}#
   if(m == month(temp1$dr_start[e]) & m == month(temp1$dr_end[e])& is.null(days_dr) ){
    # days_dr = as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))}
      mat[years_of_drought[which(!is.na(years_of_drought))], m] =  as.numeric(ymd(temp1$dr_end[e])) - as.numeric(ymd(temp1$dr_start[e]))}
   if(m == month(temp1$dr_start[e]) & is.null(days_dr)){
     #days_dr = as.numeric(days_in_month(m) - day(ymd(temp1$dr_start[e])))}
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = as.numeric(days_in_month(m) - day(ymd(temp1$dr_start[e])))}
   if(m == month(temp1$dr_end[e]) & is.null(days_dr)){
     #days_dr = as.numeric(day(ymd(temp1$dr_end[e])))}
     mat[years_of_drought[which(!is.na(years_of_drought))], m] = as.numeric(day(ymd(temp1$dr_end[e])))}
     
   if(year(ymd(temp1$dr_start[e])) < year(ymd(temp1$dr_end[e])) & m < tail(months,1) & is.null(days_dr)){
     #days_dr = days_in_month(m) %>% as.numeric()
      cat(e, "row", i, "catchment", m, "\n")
      mat[years_of_drought[which(!is.na(years_of_drought))], m] = days_in_month(m)
   }
   # if(m == 1)  cat(days_dr,e, "\n")
  
  
     
   #days_catch = rbind(days_catch, days_dr) #days per catchment effected by drought #
   def_catch = rbind(def_catch,temp1$def_vol[e]*days_dr) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them proportional to the amount of days effected
   print(mat,"\n")#
   #print(temp1[e,], "\n") #
   Sys.sleep(10)#
 
   # if(is.null(days_dr)) {
   #  # stop(e, "row", i, "catchment")
   #   cat(e, "row", i, "catchment", m, "\n")}
 }else{
   next
 }
}
  


sum_def[m] = round(sum(def_catch),0) #sum deficit per catchment per month
mean_n[m] = length(def_catch) # mean number of events per month per catchment
#sm_length[m] = sum(days_catch) #total sum of days effected in every month per catchment #


}
return(list(sum_def, mean_n,mat))
#    return(cbind(sum_def, mean_n,sm_length)) #

}


cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:catch_n, .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ 
   sub_80th(i=c)
                        }
close(pb)
stopCluster(cl)
#save(res,file="./output/seasonal_q.Rdata")

sum_def_list <- lapply(res, function(x) x[,1])
sum_def_df = do.call( "cbind",sum_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
mean_n_list <- lapply(res, function(x) x[,2])
mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
sm_days_list <- lapply(res, function(x) x[,3])
sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catch_n)

return(list(sum_def_df,mean_n_df,sm_days_df))
}

#seasonal yearly calculation ####
####yearly trends in droughts####
yearly_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th_yearly =  function(i) {

temp1 = data %>%
  filter(catchment == i)
  
mat_def = matrix(nrow=length(year_ta), ncol=1, data = 0)
mat_days = matrix(nrow=length(year_ta), ncol=1, data = 0)


  def_catch=NULL
  days_catch=NULL
for (e in 1:max(temp1$event_no)){
  days_dr = NULL
  years = NULL
  #retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought (in days)
  mt_yr = NULL
   mt_yr = seq.Date(from=ymd(temp1$dr_start[e]), to= ymd(temp1$dr_end[e]), by="month")
   if(month(ymd(temp1$dr_start[e])) !=  month(ymd(temp1$dr_end[e]))){
     mt_yr = c(mt_yr, ymd(temp1$dr_end[e]))
   }
     year_y = year(mt_yr) - (min(year_ta)-1)

     mat_def[year_y] = sum(mat_def[year_y],)
     
if(all(is.na(mat_def[cbind(year_y, month_x)]))){
   if(length(month_x)>2){ 
    mat_def[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = temp1$def_vol[e]*as.numeric(days_in_month(month_x[2:(length(month_x)-1)]))
    mat_days[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = days_in_month(month_x[2:(length(month_x)-1)]) %>% as.numeric()
   }
    mat_def[year_y[1],month_x[1]] = temp1$def_vol[e]*(30-day(ymd(temp1$dr_start[e])))
    mat_def[year_y[length(year_y)],month_x[length(month_x)]] = temp1$def_vol[e]*day(ymd(temp1$dr_end[e]))
    mat_days[year_y[1],month_x[1]] = 30-day(ymd(temp1$dr_start[e]))
    mat_days[year_y[length(year_y)],month_x[length(month_x)]] = day(ymd(temp1$dr_end[e]))
 
 }else{ # if there was already a drought in that month the new drought event needs to be added to the drought (that already occured in that month). Happens where there are two short droughts right after each other. 
   
   if(length(month_x)>2){ 
     mat_def[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = temp1$def_vol[e]*as.numeric(days_in_month(month_x[2:(length(month_x)-1)]))
    mat_days[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = days_in_month(month_x[2:(length(month_x)-1)]) %>% as.numeric()
   }
    mat_def[year_y[1],month_x[1]] = sum(mat_def[year_y[1],month_x[1]],temp1$def_vol[e]*(30-day(ymd(temp1$dr_start[e]))))
    mat_def[year_y[length(year_y)],month_x[length(month_x)]] = sum(mat_def[year_y[length(year_y)],month_x[length(month_x)]],temp1$def_vol[e]*day(ymd(temp1$dr_end[e])))
    mat_days[year_y[1],month_x[1]] = sum(mat_days[year_y[1],month_x[1]],30-day(ymd(temp1$dr_start[e])))
    mat_days[year_y[length(year_y)],month_x[length(month_x)]] = sum(mat_days[year_y[length(year_y)],month_x[length(month_x)]],day(ymd(temp1$dr_end[e])))
 
 }

sum_def[which(y == year_ta)] = round(sum(def_catch),0) #sum_deficit per year per catchment
mean_n[which(y == year_ta)] = length(def_catch)#number of events per year per catchment
sm_length[which(y == year_ta)] = sum(days_catch) #total sum of days per year per catchment


}
return(cbind(sum_def, mean_n,sm_length))
}


cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:catch_n, 
                        .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ 
   sub_80th_yearly(i=c)
                        }
close(pb)
stopCluster(cl)
#save(res,file="./output/seasonal_q.Rdata")

sum_def_list <- lapply(res, function(x) x[,1])
sum_def_df = do.call( "cbind",sum_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
mean_n_list <- lapply(res, function(x) x[,2])
mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
sm_days_list <- lapply(res, function(x) x[,3])
sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catch_n)

return(list(sum_def_df,mean_n_df,sm_days_df))
}

yearly_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th_yearly =  function(i) {

  sum_def=c()
  mean_n = c()
  sm_length = c(year_ta)
  
  
    temp1 = data %>%
    filter(catchment == i)
    
for (y in year_ta){
  def_catch=NULL
  days_catch=NULL
for (e in 1:max(temp1$event_no)){
  days_dr = NULL
  years = NULL
    if(year(temp1$dr_start[e]) == year(temp1$dr_end[e])){
      years = year(temp1$dr_start[e])}
   if((year(temp1$dr_end[e]) > year(temp1$dr_start[e]))){
     years = seq(from = year(temp1$dr_start[e]), to = year(temp1$dr_end[e]), by=1)
   }

#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought. if the drought is longer than one year the cumulative sum of the deficit volume gets devided by number of years (including partial years)
   
  
 if(y %in% years){
     #calculating number of days that are affected by drought
    if(length(years) > 1 & year(ymd(temp1$dr_start[e])) == y){
  days_dr = as.numeric (365 - yday(temp1$dr_start[e]))
         }
   if(length(years) > 1 & year(ymd(temp1$dr_end[e])) == y){
  days_dr = as.numeric(yday(temp1$dr_start[e]))
   }
    if(length(years) > 2 & is.null(days_dr)){
  days_dr = 365
       } 
    if(length(years) == 1){
      days_dr = as.numeric(ymd(temp1$dr_end[e]))-as.numeric(ymd(temp1$dr_start[e]))
       } 
      
   def_catch = rbind(def_catch,temp1$def_vol[e]*days_dr) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them.the deficit is shared proportionally among the effected years 
  
 
    days_catch = rbind(days_catch, days_dr)
   # if(is.null(days_dr)|is.null(years)) {
   #   #stop(e, "row", i, "catchment")
   #   cat(e, "row", i, "catchment", "\n")}
 }else{
   next
 }
 }  

sum_def[which(y == year_ta)] = round(sum(def_catch),0) #sum_deficit per year per catchment
mean_n[which(y == year_ta)] = length(def_catch)#number of events per year per catchment
sm_length[which(y == year_ta)] = sum(days_catch) #total sum of days per year per catchment


}
return(cbind(sum_def, mean_n,sm_length))
}


cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:catch_n, 
                        .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ 
   sub_80th_yearly(i=c)
                        }
close(pb)
stopCluster(cl)
#save(res,file="./output/seasonal_q.Rdata")

sum_def_list <- lapply(res, function(x) x[,1])
sum_def_df = do.call( "cbind",sum_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
mean_n_list <- lapply(res, function(x) x[,2])
mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
sm_days_list <- lapply(res, function(x) x[,3])
sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catch_n)

return(list(sum_def_df,mean_n_df,sm_days_df))
}


# functions ---------------------------------------------------------------

dist_fitt <- function(distry, monthy){ #similar as above, old version, not used in script
  q_by_month <- data.frame()
    for (i in monthy){
    q_by_month <- month_ext(monthx = monthy)
    assign(str_to_lower(month.abb[i]), q_by_month)
    }
    temp <- fitSCI(q_by_month$V1, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
  assign(paste0("params_", disrty), temp)
}


