
# Masterarbeit ------------------------------------------------------------


# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
#save.image(file="./data/r_temp_image/line307.Rdata")
# load(file="./data/r_temp_image/line307.Rdata")

#install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "SPEI", "eha","reliaR", "PearsonDS","FAdist","trend", "Kendall"))
# install.packages("drought", repos="http://R-Forge.R-project.org")
# install.packages(c("trend", "Kendall"))
# devtools::install_github("hadley/dplyr")
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "lubridate", "SPEI", "lmomco",  "evd", "reliaR", "PearsonDS", "FAdist","trend","Kendall"), require, character.only = T)
#library(eha) drought

# User defined functions --------------------------------------------------

sci_calc <- function(datax = spei_data$p_pet, gaugex=spei_data$gauge, distx="gev", agg_n=6, p0x=F){ #fit monthly values to certain distribution and calculate indice
 # 1. Step: fitting of distribution to aggregated data with SCI package
 output <- as.list(NA)
for(i in unique(gaugex)){
data <- datax[gaugex==i]
params_dist <- fitSCI(data, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
#inital values for parameters calculated with L-moments and then optimization with maximum likelihood
#if there are a lot of zeros in the time series it is recommended to use p0=TRUE
# 2. Step: transform to standart normal distribuion with mean: 0 and sd: 1
spi_temp <- transformSCI(data, first.mon = 1, obj = params_dist)
output[[i]] <- spi_temp
}
 return(output)
}

# dist_fitt <- function(distry, monthy){ #similar as above, old version, not used in script
#   q_by_month <- data.frame()
#     for (i in monthy){
#     q_by_month <- month_ext(monthx = monthy)
#     assign(str_to_lower(month.abb[i]), q_by_month)
#     }
#     temp <- fitSCI(q_by_month$V1, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
#   assign(paste0("params_", disrty), temp)
# }

spei_vec <- function(data, spei=FALSE, gaugex=mt_mn_temp$gauge){ #to transform spei or spi list into vector
  output <-  vector()
  for(i in unique(gaugex)){
  temp <- unclass(data[[i]])
  if(spei == FALSE){
    output <- c(output, as.numeric(temp))}else{
    output <- c(output, as.numeric(temp$fitted))}
  }
  return(output)
}


month_ext <- function(monthx = 1, datax = mt_mn_q, yr_llim=1970, yr_rlim = 2000){#month extraction for SSI calculation to calculate SSI for each month individually
    output <- data.frame()
    data <- datax %>% 
      filter(month == monthx)
    for (g in unique(data$gauge)){
      for (y in seq(yr_llim,yr_rlim,by = 1)){
        output[y,g] <- data$q_mean[data$gauge == g & year(data$yr_mt) == y]
      }
    }
    output_short <- output[yr_llim:yr_rlim,]
    return(output_short)
}
# Load data ---------------------------------------------------------------

load("./data/catchments/eobs_pr_part.Rdata")
load("./data/catchments/eobs_temp_part.Rdata")
load("./data/catchments/streamflow.Rdata")
#gauges  <- readOGR(dsn="./data/raster/gauges", layer= "gauges")
gauges  <- shapefile("./data/raster/gauges")

raster(gauges)
# overview ----------------------------------------------------------------
#gauges
# plot(gauges) 
# str(gauges)

#writing loading function

load_file <- function(file, value_name, origin="1970-1-1"){
  output <- melt(file, varnames = c("date", "gauge"), value.name = value_name )
  seq_date <- seq.Date(from= as.Date(origin),by=1, length.out = diff(range(output$date))+1) %>% 
  rep(., times=length(unique(output$gauge)))
  output %<>%
  mutate(date = seq_date) %>% 
  mutate(gauge = parse_number(gauge)) %>% 
  mutate(gauge =as.integer(gauge)) %>% 
  as.tibble()
  return(output)
}

#precip
precip_long <- load_file(precip, "sum_mm")

# precip_long %>% 
#   filter(gauge < 10) %>%
#   ggplot()+
#     geom_smooth(aes(x=date, y=sum_mm, colour=as.factor(gauge), group=gauge), se=F)


#discharge
q_long <- load_file(streamflow, "q")

# q_long %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=q, colour=as.factor(gauge), group=gauge), se=F)

#temperature
temp_long <- load_file(file=tempera, value_name = "temp", origin = "1950-01-01")
temp_long %<>% filter(date>= "1970-01-01" & date <= "2009-12-31") 


# temp %>% 
#   filter(gauge < 10) %>%
# ggplot()+
#   geom_smooth(aes(x=date, y=temp, colour=as.factor(gauge), group=gauge), se=F)



# # Cluster calculation -----------------------------------------------------
# #seasonality ratio (SR)
# 
# # creating two time series one winter one summer
# #calculating q95 for both parts
# q_sr_w <- q_long %>% 
#   mutate(month = month(date)) %>% 
#   filter(month > 11 | month <4) %>%  #definitin of winter from Laaha et al 2006
#   group_by(gauge) %>% 
#   mutate(qt = quantile(q, 0.05)) %>% 
#   summarise(q95_w = mean(qt))
# 
# q_sr_s <- q_long %>% 
#   mutate(month = month(date)) %>% 
#   filter(month < 12 | month > 3) %>% 
#   group_by(gauge) %>% 
#   mutate(qt = quantile(q, 0.05)) %>% 
#   summarise(q95_s = mean(qt))
# 
# q_sr <- merge(q_sr_s, q_sr_w, by="gauge")
# q_sr$sr <- q_sr$q95_s/q_sr$q95_w # SR is caclulated via q95_summer/q95_winter from Laaha et al 2006
# 
# q_sr$sr_value[which(q_sr$sr < 1)] <- 0 #summer
# q_sr$sr_value[which(q_sr$sr > 1)] <- 1 #winter NAs are produced in a 9 time series that are very altered or have no clear seasonality 
# 
# gauges$sr <- as.numeric(q_sr$sr_value)
# # spplot(gauges, "sr")
#   
# # ind <- which(is.na(q_sr$sr_value))
# # q_long %>%
# #   filter(gauge %in% ind) %>%
# #   filter(year(date) < 1975) %>%
# #   ggplot() +
# #   geom_smooth(aes(x=date, y=q, group= as.factor(gauge), color= as.factor(gauge)))+
# #   scale_y_log10()
# 
# 
# SPI calculation ---------------------------------------------------------
#aggregating into montly sums

precip_monthly <- precip_long %>%
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>%
  group_by(gauge,yr_mt) %>%
  summarise(month_sum = sum(sum_mm)) %>%
  ungroup()

# plot(spi[[1]], type="l")

#calculating SPI with gamma distribution see paper McKee et al 1993
for (i in 1:12){ #change to 24 later
  #SPI - n aggregation month 1 -24 month like in barker et al 2016
temp <- sci_calc(datax = precip_monthly$month_sum, gaugex =precip_monthly$gauge, distx = "gamma", agg_n = i )
spi_v <- spei_vec(temp)
m1 <- matrix(spi_v, nrow = 480, byrow =F)
spi_df <- as.data.frame(m1) 
assign(paste0("spi_",i), spi_df)
}

# SPEI Calculation --------------------------------------------------------
#with SPEI package and SCI


# PET calculation with thornwaite -----------------------------------------

mt_mn_temp <- temp_long %>%
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>%
  group_by(gauge, yr_mt) %>%
  summarise(temp_m = mean(temp)) %>%
  ungroup()


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
spei_data <-precip_monthly %>%
    mutate(pet_th = pet_th_vec) %>%
    mutate(p_pet = month_sum - pet_th)



# Calculatin SPEI with different distributions ----------------------------

# with Generalized Logistic Distribution
spei_gl <- sci_calc(distx = "genlog")

#with Generalized extreme value distribution
spei_gev <- sci_calc()

#with gamma distribution
spei_gam <- sci_calc(distx="gamma") #bad result three parameter distribution used for arid regions because it can handle negative values better vicente-serrano et al 2010
#converting SPEI objects into vectors

# spei_gv_v <- spei_vec(spei_gev)
# spei_gl_v <- spei_vec(spei_gl)

# m1 <- matrix(spei_gl_v, nrow = 480, byrow =F)
# spi_df <- as.data.frame(m1) 

# SPEI calculation with loglogistic distribution--------------------------------
#(different function from SPEI package)
spei_llogis <- as.list(NA)
for (n in 1:12){ 
  #SPEI - n aggregation month 1 - 12 month like in barker et al 2016 but SPI is 1:14 careful!! change later
for(i in unique(spei_data$gauge)){
data <- spei_data$p_pet[spei_data$gauge==i]
spei_llogis[[i]] <- spei(data, scale=n)
}
m1 <- matrix(spei_vec(spei_llogis, spei = TRUE), nrow = 480, byrow =F)
assign(paste0("spei_",n), as.data.frame(m1))
}  




# Distribution comparison SPEI -----------------------------------

#merging data

spei <- cbind(spei_ll_v, spei_gv_v, spei_gl_v) %>%
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
  ks.test(x=spei_data$p_pet, y=spei$spei_ll_v, alternative = "t" )
  ks.test(x=spei_data$p_pet, y=spei$spei_gv_v, alternative = "t" )
  ks.test(x=spei_data$p_pet, y=spei$spei_gl_v, alternative = "t" )
  # similar d values the lower the better

  
# SSI calculation ---------------------------------------------------------
mt_mn_q <- q_long %>% 
    mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>% 
  group_by(gauge,yr_mt) %>% 
  summarise(q_mean = mean(q)) %>% 
  ungroup() %>% 
  mutate(month = month(yr_mt))
  
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
  


# mean discharge extraction for every gauge -------------------------------


# for (i in 1:12){
#   q_by_month <- month_ext(monthx = i, yr_rlim = "2009", datax = mt_mn_q)
#   assign(paste0("q_",str_to_lower(month.abb[i])), q_by_month)
# }


  
# using non parametric solution for SSI -----------------------------------

#fitting to cumulative normal distribution with mean 0 and sd = 1

  gauges_ssi <- data.frame()
  ssi_entire <- data.frame()

  for (i in 1:12){
     data <- month_ext(monthx = i, yr_rlim = "2009", datax = mt_mn_q)
     for (g in 1:ncol(data)){
     gauge <- data[,g]
     stan <- (gauge-mean(gauge))/sd(gauge)
     gauges_ssi[(1:length(gauge)),g] <- stan
     }
    gauges_ssi$yr_mt <- seq.Date(from= ymd(paste("1970",i, "15", sep="-")), to = ymd(paste("2009",i,"15", sep="-")), by="year")  
    ssi_entire <- rbind(ssi_entire,gauges_ssi)
  } 
ssi_sorted <- ssi_entire[order(as.Date(ssi_entire$yr_mt)),]
remove(ssi_entire)
# the standartization (p.42p in satistical methods in the atmospheric sciences) removes the interannual variation forms a distribution with mean 0 and sd 1 but it does not remove the skewness (see hist(janssi[,6]) it is still skewed to the right
#if not n+1 than that would mean that the probability to measure an event higher than the highest is 0


# using parametric method for SSI -----------------------------------------


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

# kendall trend -----------------------------------------------------

mk_spi_tau <- list()
mk_spi_S <- list()
mk_spi_D <- list()
mk_spi_p <- list()
for (i in 1:ncol(spi_df)){
 mk_spi_tau[[i]] <-  Kendall(spi_df[,i], ssi_sorted[,i])$tau[1] 
 mk_spi_S[[i]] <- Kendall(spi_df[,i], ssi_sorted[,i])$S[1] 
mk_spi_D[[i]] <- Kendall(spi_df[,i], ssi_sorted[,i])$D[1] 
mk_spi_p[[i]] <- Kendall(spi_df[,i], ssi_sorted[,i])$sl[1] 
}

Kendall(spi_df[,i], ssi_sorted[,i])$sl[1]
#tau ist S/D
#S anzahl an positiven - negativen trends
#D value theoretisch mögliche maximale anzahl an trends
plot(x =spi_df[,1] , y=ssi_sorted[,1])
#offensichtliche aussage: desto höher der spi, desto höher der spei

# autocorrelation ---------------------------------------------------------
vec
acf(spei_1)


# cross correlation -------------------------------------------------------

ccf(x= spei_7$V1, y= ssi_sorted$V1, na.action = na.pass)
#one can see that advancing the spei value leads to considerly less correlation after the spei_n lag (positive) the correlation drops dramatically. retropespective the correlation decreases slower. Meaning that spei leads ssi. 
plot(x= spei_6$V1, y= ssi_sorted$V1)
plot(ssi_sorted$V1, type="l")
