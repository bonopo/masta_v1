
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



# decompose time series into trend and seasonal part ---------------------

install.packages("fpp")
require(fpp)
ts = ts(mt_mn_q_wide, start=c(1970,1), end=c(2009,12), deltat=1/12)
ssi_dec <- decompose(ts[,1])
plot(ssi_dec)
res=decompose(ts)
plot(res)
res$trend[,1] %>% plot()
stl_res = stl(ts[,1], "periodic")
str(stl_res)
trend = stl_res$time.series[,2]
seas = stl_res$time.series[,1]
dum_var = cbind(trend, seas)
y= ts[,1]
fit2 = tslm(y ~ trend + season)
n <- length(y)
plot(y)
lines(ts(fit2$coef[1]+fit2$coef[2]*(1:n)+mean(fit2$coef[-(1:2)]),
  start=start(y),f=12),col="red")

res = arima(y, xreg =dum_var) %>% summary()

str(res)
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


#predictor analysis with randomForest####
install.packages("randomForest")
library(randomForest)
hist(dat)
head(dat)
cart = randomForest::randomForest(data= dat, y~.)
summary(cart)
varImpPlot(cart)
round(importance(cart),2)
plot(mmky_ms7_min$sen_slope ~ mmky_su_mn_t$sen_slope)
getTree(cart, k=500, labelVar = T)
plot.randomForest(cart)

#median drought duration ####
#not useful because drought length is always a multiple of a month 
      #   median_drought_duration = c()
      #   for (g in 1:catch_n){
      #     median_drought_duration[g] = dsi_1[[g]]$dr_length %>% median()
      #   }
      # 
      # gauges$med_dr_dur = median_drought_duration
      # remove(median_drought_duration)
#maximum duration ####
#not useful because drought length is always a multiple of a month 
    # max_drought_duration = c()
    #   for (g in 1:catch_n){
    #     max_drought_duration[g] = dsi_1[[g]]$dr_length %>% max()
    #   }
    # 
    # gauges$max_dr_dur = max_drought_duration
    # remove(max_drought_duration)
#median severity####
#not useful because drought length is always a multiple of a month 

    # median_drought_severity = c()
    #   for (g in 1:catch_n){
    #     median_drought_severity[g] = dsi_1[[g]]$dsi %>% median()
    #   }
    # 
    # gauges$med_dr_sev = median_drought_severity
    # remove(median_drought_severity)
# #median intensity ####
#not useful because drought length is always a multiple of a month 
    # 
    # median_drought_inten = c()
    #   for (g in 1:catch_n){
    #     median_drought_inten[g] = dsi_1[[g]]$dr_intens %>% median()
    #   }
    # 
    # gauges$med_dr_int  =median_drought_inten
    # remove(median_drought_inten)
# #maximum severity####
#not useful because drought length is always a multiple of a month 
    # max_drought_sev = c()
    #   for (g in 1:catch_n){
    #     max_drought_sev[g] = dsi_1[[g]]$dsi%>% min() #min!!!!!!!!!!!
    #   }
    # 
    # gauges$max_dr_sev  =max_drought_sev
    # remove(max_drought_sev)

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


