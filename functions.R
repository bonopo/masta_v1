
# User defined functions --------------------------------------------------

# loading function####

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

month_ext <- function(monthx = 1,datax = mt_sm_p_long, value="month_sum"){#month extraction for SSI calculation to calculate SSI for each month individually
    output <- data.frame()
    col_no <- which(colnames(datax) == value)
    data <- datax %>% 
      mutate(month = lubridate::month(yr_mt)) %>% 
      filter(month == monthx)
    for (g in unique(data$gauge)){
      for (y in seq(min(year(datax$yr_mt)),max(year(datax$yr_mt)),by = 1)){
    output[y,g] <- data[data$gauge == g & year(data$yr_mt) == y,col_no]

      }
    }
    output_short <- output[min(year(datax$yr_mt)):max(year(datax$yr_mt)),]
    return(output_short)
}
# correlation ####

sci_ccf <- function(sci= c(1,2,3,6,12,24), sci_namex="spei_", sci_namey="ssi_1"){
  ccf_tot <- list()
  ccf_acf <- data.frame()
  ccf_lag <- data.frame()
  y <- get(sci_namey)
  i=1
  for (a in sci){
  sci_data <- get(paste0(sci_namex,a))
  for(g in 1:catch_n){
  ccf_temp <- ccf(x= sci_data[,g], y= y[,g], na.action = na.pass, plot = FALSE)
  ccf_acf[g,i] <- max(ccf_temp$acf)
  ccf_lag[g,i] <- ccf_temp$lag[which.max(ccf_temp$acf),1,1]
  }
  i= i+1}
  colnames(ccf_lag) <- as.character(sci)
  colnames(ccf_acf) <- as.character(sci)
  ccf_tot[[2]] <- ccf_lag
  ccf_tot[[1]] <- ccf_acf
  return(ccf_tot)
}

cor_sci_ssi <- function(sci_n= c(1,2,3,6,12,24), cor_met="p", sci="spi_", ssi="ssi_1"){
   mat <- matrix(ncol=length(sci_n), nrow=catch_n)
   i <- 1
  for (n in sci_n){
  x_data <- get(paste0(sci,n))
  y_data <- get(ssi)
     for (g in 1:catch_n){
    mat[g,i] <- cor(x= x_data[,g], y_data[,g], method = cor_met, use="na.or.complete" )
     }
  i = i+1}
df = mat %>% as.data.frame()
colnames(df) = sci_n
return(df)

}

cor_sci_ssi_sea <- function(sci_n= c(1,2,3,6,12,24), cor_met="p", sci="spi_", ssi="ssi_1", begin=4, end=10){
   mat <- matrix(ncol=length(sci_n), nrow=catch_n)
   i <- 1
  for (n in sci_n){
  x_data <- get(paste0(sci,n)) %>% 
    mutate(yr_mt = as.Date(yr_mt, origin = "1970-01-01")) %>% 
    filter(month(yr_mt) >= begin | month(yr_mt)<= end) 
  y_data <- get(ssi) %>% 
    filter(month(yr_mt) >= begin | month(yr_mt)<= end) 
  for (g in 1:catch_n){
    mat[g,i] <- cor(x= x_data[,g], y_data[,g], method = cor_met, use="na.or.complete" )
     }
  i = i+1}
df = mat %>% as.data.frame()
colnames(df) = sci_n
return(df)
}

#regression ####

spi_spei_reg <- function(sci="spi_", sci_n = c(1,2,3,6,12,24)){
  lm_intercept <- data.frame()
  lm_slope <- data.frame()
  lm_rsq <- data.frame()
  lm_res <- list()
  i=1
  for (n in sci_n){
      x <- get(paste0(sci,n))
      y <- get("ssi_1")
      for (g in 1:catch_n){
        temp <- lm(x[,g]~y[,g], na.action = na.exclude)
        lm_intercept[g,i]  <- temp$coefficients[1]
        lm_slope[g,i] <- temp$coefficients[2]
        lm_rsq[g,i] <- summary(temp)$adj.r.squared
      }
      i= i+1
  }
  colnames(lm_intercept) <- sci_n
  colnames(lm_slope) <- sci_n
  colnames(lm_rsq) <- sci_n
  lm_res[[1]] <- lm_intercept
  lm_res[[2]] <- lm_slope
  lm_res[[3]] <- lm_rsq
return(lm_res)
}


sci_reg <- function(pred="spei_1", resp="ssi_1", pred2="spi_1", additive=FALSE){
  lm_res <- list()
  res <- matrix(nrow=catch_n, ncol = 4)
      x <- get(pred)
      x2 <- get(pred2)
      y <- get(resp)
      for (g in 1:catch_n){
        if(additive==FALSE){
          temp <- lm(y[,g]~x[,g] , na.action = na.exclude)
        res[g,1]  <- temp$coefficients[1]
        res[g,2] <- temp$coefficients[2]
        res[g,3] <- summary(temp)$adj.r.squared
        res[g,4] = summary(temp)$coefficients[2,4]
        }else{
        temp <- glm(y[,g]~x[,g] +x2[,g], na.action = na.exclude) 
        res[g,1]  <- temp$coefficients[1]
        res[g,2] <- temp$coefficients[2]
        res[g,3] <- summary(temp)$adj.r.squared
        res[g,4] = summary(temp)$coefficients[2,4]
        
        }
      }
      res <- as.data.frame(res)
      colnames(res) <- c("intercept", "slope", "rsq")
       return(res)
    }




# non parametric sci calculation ------------------------------------------
a=1
sci_np <- function(sci_data="mt_sm_p_wide", agg_n=1, sci_name="spi"){
 for (a in agg_n){
  erg <- matrix(nrow=480, ncol=(catch_n+1))
  data <- get(sci_data)
   if(a>1){ 
   data <- rollapply(data, width=a, FUN=mean, by.column =TRUE, align="right", fill=NA) %>% as.data.frame()}
  data$yr_mt <- date_seq
    for(i in 1:catch_n){
    for (m in 1:12) {
    data_temp <- data %>% filter(month(yr_mt) == m) %>% dplyr::select(i) 
    data_temp_na <- data_temp[which(!is.na(data_temp)),] #removing NA, produced by moving average
      erg[(40*m-39):(40*m),i] <-  c(rep(NA,times=length(which(is.na(data_temp)))),qnorm(trunc(rank(data_temp_na))/(length(data_temp_na)+1))) # fitting data to follow normal inverse cumulative function
      erg[(40*m-39):(40*m),(catch_n+1)] <- data$yr_mt[month(data$yr_mt)==m] # adding date to sort later
    }}
 erg <- erg[order(as.Date(erg[,(catch_n+1)])),] %>% as.data.frame()
 colnames(erg) <- c(1:catch_n,"yr_mt")
 erg$yr_mt = as.Date(erg$yr_mt, origin = "1970-01-01")
 assign(paste0(sci_name,"_",a), erg, envir = .GlobalEnv)
 }}

# drought characteristics -------------------------------------------------


#counting every month below threshhold
dr_n <- function(severity = -1)  {
try(if(severity < min(ssi_1_long$ssi)) stop ("Too low severity. Choose higher SSI Value!!!!!"))
 res<- list()
for (i in 1:catch_n){
  ssi_temp <- ssi_1_long %>% 
   mutate( year= year (yr_mt))%>% 
   filter(ssi < (severity)) %>% 
   as.tbl()
  
  res[[i]] <-   ssi_temp %>% 
    filter(gauge== i) %>% 
    group_by(year) %>% 
    summarise(occurences = n())
}
 return(res)
}

#counting number of events depending on severity threshhold

dr_count <- function(severity = -1.5){
  try(if(severity < min(ssi_1_long$ssi)) stop ("Too low severity. Choose higher SSI Value!!!!!"))
 res<- list()
 for (g in 1:catch_n){
s1 <- ssi_1_long %>% 
  filter(gauge == g && ssi < severity) %>% 
   mutate(date_diff = c(diff.Date(yr_mt),0)) 
n <- 1
for (i in 1: length(s1$yr_mt)){
  s1$event_n[i] <- n
if(s1$date_diff[i] > 31) {
  n <- n+1
}}
res[[g]] <- s1}
 return(res)
}

#sum of severity per event

dr_severity <- function(severity = -1, data_source = "dr_event_no"){
try(if(severity < min(ssi_1_long$ssi)) stop ("Too low severity. Choose higher SSI Value!!!!!"))
res_list <- list()
for (g in 1:catch_n){
    data  <- get(data_source)[[g]]
    res   <- matrix(nrow = max(data$event_n), ncol=6) 
for (d in 1:max(data$event_n)){
  res[d,1]        <- sum(data$ssi[data$event_n == d]-severity)
  res[d,2]        <- d
  res[d,3]        <- data$yr_mt[data$event_n == d][1] 
  res[d,4]        <- tail(data$yr_mt[data$event_n == d],1)
 if(res[d,4] - res[d,3] == 0){
     res[d,5] =  days_in_month(as.Date(res[d,3], origin = "1970-01-01"))}else{
       res[d,5] =  res[d,4] - res[d,3]
     }
  res[d,6] = res[d,1]/((month(res[d,4]) - month(res[d,3]))+1)
  }
    colnames(res) <- c("dsi", "event_n", "dr_start", "dr_end", "dr_length", "dr_intens")
    res <- as.data.frame(res)
    res$dr_start<- as.Date(res$dr_start, origin = "1970-01-01")
    res$dr_end<- as.Date(res$dr_end, origin = "1970-01-01")
    res_list[[g]] <- res
    }
return(res_list)
}

#plot functions ####

#regression sci ~ ssi
plot_reg <- function(spi_source = "spi_ssi_reg", spei_source="spei_ssi_reg", agg_n = 1){
  spi <- get(spi_source)
  spei <- get(spei_source)
  print({
pdf(paste0("./plots/reg_slope_",agg_n,".pdf"))
  plot(x=1:catch_n, y=spi$slope, type="p", ylim=c(0,1), ylab="lm slope", xlab="catchment")
  points(spei$slope, col=2)
  legend("topleft", col=c(1,2), pch=c(1, 1), bty="n", c("SPI", "SPEI"))})
dev.off()
  print({
pdf(paste0("./plots/reg_intercept_",agg_n,".pdf"))
  plot(x=1:catch_n, y=spi$intercept, type="p", ylab="lm intercep", xlab="catchment")
  points(spei$intercept, col=2)
  legend("topleft", col=c(1,2), pch=c(1, 1), bty="n", c("SPI", "SPEI"))})
dev.off()
  print({
pdf(paste0("./plots/reg_rsq_",agg_n,".pdf"))
  plot(x=1:catch_n, y=spi$rsq, type="p", ylim=c(0,.8), ylab="lm r²", xlab="catchment")
  points(spei$rsq, col=2)
  legend("topleft", col=c(1,2), pch=c(1, 1), bty="n", c("SPI", "SPEI"))})
dev.off()
}


# trend calculaton --------------------------------------------------------
qua_trend <- function(quantil=0.1){
  ken=matrix(nrow=catch_n, ncol=2)
  data <- q_long %>%
  mutate(year = year(date)) %>% 
  group_by(gauge, year) %>%
  summarise(qt=quantile(q, quantil)) %>% 
    ungroup()
  for (g in 1:catch_n){
  res <- MannKendall(data[data$gauge == g,]$qt)
  ken[g,1] <- res$tau
  ken[g,2] <- res$sl
  }
  ken <- as.data.frame(ken)
  colnames(ken) <- c("tau", "sl")
  return(ken)
  }

ken_trend <- function(sci= seq(1:agg_month), sci_name="spei_v2"){
  ken_tot <- list()
  ken_tau <- data.frame()
  ken_sl <- data.frame()
  for (i in sci){
  if(sci_name == "ssi"){
  sci_data <- ssi_sorted }else{
  sci_data <- get(paste0(sci_name,"_",i))}
  for(g in 1:ncol(sci_data)){
  res <- MannKendall(sci_data[,g])
  ken_tau[g,i] <- res$tau
  ken_sl[g,i] <- res$sl
  }}
  if(sci_name == "ssi"){
    ken_tot <- cbind(ken_tau[,1], ken_sl[,1])
    colnames(ken_tot) <- c("tau", "sl")
  }else{
  colnames(ken_tau) <- as.character(sci)
  colnames(ken_sl) <- as.character(sci)
  ken_tot[[1]] <- ken_tau
  ken_tot[[2]] <- ken_sl}
  return(ken_tot)
}

## extracting seasonal data 
seas_cl = function(data_source = "mt_mn_temp", method="mean", value = "temp_m", begin =4, end=10){
 data = get(data_source)
  if(method=="mean"){
  res <- data %>% 
  filter(month(yr_mt) >= begin | month(yr_mt)<= end) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean = get(method)(get(value)))  %>% 
  ungroup() %>% 
  spread(key=gauge, value=method) %>% 
  as.data.frame()
}else{
res <- data %>% 
  filter(month(yr_mt) >= begin | month(yr_mt)<= end) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(value = get(method)(get(value))) %>% 
  ungroup() %>% 
  spread(key=gauge, value=value) %>% 
  as.data.frame()
}
    return(res)
}




## drought calculation with long term average deviation after Parry et al 2016 ####

# d = min. number of days with negative Z to start "drought"
#r= allowed wet days within a drought e.g. so that the drought is not ended by one peak flow one day
#t= when z is positive for specific (t) number of consecutive time steps -> drought ends
#t_et drought termination 
#tm= drought termination magnitude = Z%anom i at  t_et
# t_sd = is the start of drought development
#t_ed is the end of drought development
#t_st is the start of drought termination


lta_dr <- function(data_source = "q_long", ref_begin = "1970", ref_end= "2000", d_start = 20, d_end = 10){
  data <- get(data_source)
  lta_tbl = data %>%
    filter(year(date)<=ref_end && year(date)>=ref_begin) %>% 
    mutate(mt_dy = paste0(month(date), "-",day(date))) %>% #ref_end,"-",
    group_by(gauge,mt_dy) %>% 
    summarise(mean_daily = mean(q)) %>% 
    ungroup()
  
  for (g in 1:catch_n){
data$lta[data$gauge == g] = lta_tbl$mean_daily[match(paste0(month(data$date[data$gauge == g]),"-", day(data$date[data$gauge == g])),lta_tbl$mt_dy[lta_tbl$gauge == g])]

data$z_anom[data$gauge == g] = 100*((data$q[data$gauge == g]/data$lta[data$gauge == g])-1)

data$z_anom_start = rollapply(data$z_anom[data$gauge==g], width=d_start, FUN= max, align="right", fill=NA)

data$z_anom_end = rollapply(data$z_anom[data$gauge==g], width=d_end, FUN= min, align="right", fill=NA)
  }
 end_data = c()
 n=1
 d=30
  for(d in 1:length(data$gauge[data$gauge == g])){
if(data$z_anom_start[data$gauge == g][d+d_start-1] < 0){
  pos_end = which(data$z_anom_end[data$gauge == g] > 0)
  end_data[n] =  data$date[pos_end[which(pos_end > d)[1]]]
  n = n+1
}

  end_data = as.Date(end_data,origin = "1970-1-1")
}
data$z_anom_end[data$gauge == g]

}
plot(y= data$z_anom[data$gauge==1],x = data$date[data$gauge==1],t="l")


