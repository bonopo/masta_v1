
# SCI calculation ---------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
source("./R/masta_v1/data_handling.R")
source("./R/masta_v1/functions.R")

# SPI calculation ---------------------------------------------------------
#aggregating into montly sums



# plot(spi[[1]], type="l")

#calculating SPI with gamma distribution see paper McKee et al 1993
# for (i in 1:agg_month){ #change to 24 later
#   #SPI - n aggregation month 1 -24 month like in barker et al 2016
# temp <- sci_calc(datax = precip_monthly$month_sum, gaugex =precip_monthly$gauge, distx = "gamma", agg_n = i )
# spi_v <- spei_vec(temp)
# m1 <- matrix(spi_v, nrow = 480, byrow =F)
# spi_df <- as.data.frame(m1) 
# assign(paste0("spi_",i), spi_df)
# }

#calculate SPI with spei package####

for (n in 1:agg_month){ 
res <- SPEI::spi(data= mt_sm_p_wide, scale=n)
m1 <- matrix(as.numeric(unclass(res)$fitted), nrow = 480, byrow =F)
if(any(is.infinite(m1))) {
     m1[which(is.infinite(m1))] <- NA}
assign(paste0("spi_v2_",n), as.data.frame(m1))
}


# SPEI Preperation --------------------------------------------------------
#with SPEI package and SCI


# PET calculation with thornwaite -----------------------------------------




#Gauss Krueger converted to WGS84
xy_gk <- cbind.data.frame("x_gk" = 4475806, "y_gk"= gauges$Hochwrt) #only latitude is needed, therefore random x value
coordinates(xy_gk) <-  c("x_gk", "y_gk")
proj4string(xy_gk) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
xy_wgs84 <- spTransform(xy_gk, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


latitude <- coordinates(xy_wgs84) %>%
  as.data.frame() %>%
  dplyr::select(y_gk) %>%
  cbind("gauge" = unique(mt_mn_temp$gauge)) %>%
  as.tbl()

#calculating Potential Evapotranspiration with thornthwaite
pet_th <- list(NA)
for(i in unique(mt_mn_temp$gauge)){
  data <- mt_mn_temp$temp_m[mt_mn_temp$gauge==i]
  res_ts <- ts(data, frequency=12, start=c(1970,1))
  pet_th[[i]] <- thornthwaite(data,latitude$y_gk[latitude$gauge==i] )
}


# calculating SPEI: P - PET -----------------------------------------------

pet_th_vec <- pet_th[unique(mt_mn_temp$gauge)] %>%
  unlist() %>%
  as.numeric()

spei_data <- mt_sm_p %>%
    mutate(pet_th = pet_th_vec) %>%
    mutate(p_pet = month_sum - pet_th) 

spei_data_mat <- spei_data %>% dplyr::select(gauge,yr_mt, p_pet) %>% spread(gauge, p_pet) %>% dplyr::select(-yr_mt) %>% as.data.frame()
colnames(spei_data_mat) <- 1:catch_n


remove(spei_data,pet_th, latitude,pet_th_vec)

# SPEI calculation with loglogistic distribution--------------------------------


for (n in 1:agg_month){ 
res <- SPEI::spei(data= spei_data_mat, scale=n)
m1 <- matrix(as.numeric(unclass(res)$fitted), nrow = 480, byrow =F)
if(any(is.infinite(m1))) {
     m1[which(is.infinite(m1))] <- NA}
assign(paste0("spei_v2_",n), as.data.frame(m1))
}

remove(m1)
# mean discharge extraction for every gauge -------------------------------
# 
# 
# for (i in 1){
#   q_by_month <- month_ext(monthx = i, datax = mt_mn_q)
#   assign(paste0("q_",str_to_lower(month.abb[i])), q_by_month)
# }
 
# SSI calculation ---------------------------------------------------------

  
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



# Calculatin SPEI with different distributions ----------------------------

# with Generalized Logistic Distribution
spei_gl <- sci_calc(distx = "genlog")

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


  
  
  
  
# Distribution free calculation -------------------------------------------
  c(6,12,24)
sci_np(sci_data="mt_sm_p_wide", agg_n=24, sci_name="spi")
sci_np(sci_data="spei_data_mat", agg_n=c(1:3,6,12,24), sci_name="spei")  
sci_np(sci_data="mt_mn_q_wide", agg_n=1, sci_name="ssi") 

plot(order(spi_24$`1`))
  
  spei_v1_s <- sci_np(sci="spei_data_mat", n=1, method = "mean") 
ssi <- sci_np(sci="mt_mn_q_wide")

plot(y= ssi$V1, x= date_seq, type="l")
lines(spi_v1$V1, col=2)

plot(spi_24$`1`[order(spi_24$`1`)])
plot(mt_sm_p_wide$`1`[order(mt_sm_p_wide$`1`)])


  ggplot()+
    geom_smooth(data= ssi, aes(x=date, y=sum_mm, colour=as.factor(gauge), group=gauge), se=F)

    p <- CTT::score.transform(mt_sm_p_wide$`2`, normalize = T)
plot(p$new.scores) 
plot(p$new.scores[order(p$new.scores)])
score
  
plot(x= spi_2_m$`1`, y=spi_2_s$`1`)
