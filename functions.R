
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
    filter(month(yr_mt) >= begin & month(yr_mt)<= end) 
  y_data <- get(ssi) %>% 
    filter(month(yr_mt) >= begin & month(yr_mt)<= end) 
  for (g in 1:catch_n){
    mat[g,i] <- cor(x= x_data[,g], y_data[,g], method = cor_met, use="na.or.complete" )
     }
  i = i+1}
df = mat %>% as.data.frame()
colnames(df) = sci_n
return(df)
}


#bfi sci analysis ####
bfi_sci =function(threshold =0){
threshold = 0
res.list=list()
l=0
mat_res = list()
bfi_class=levels(gauges$bfi_class)
for(i in bfi_class){

for (c in which(gauges$bfi_class == i)){
    l=l+1
df= cbind.data.frame(ssi = ssi_1[,c], spi_v2_1[,c], spi_v2_2[,c], spi_v2_3[,c], spi_v2_6[,c], spi_v2_12[,c], spi_v2_24[,c], spei_v2_1[,c], spei_v2_2[,c], spei_v2_3[,c], spei_v2_6[,c], spei_v2_12[,c], spei_v2_24[,c]) 
df_drought = df %>% filter(ssi < threshold)
colnames(df_drought) = c("ssi", "spi_1", "spi_2", "spi_3", "spi_6", "spi_12", "spi_24", "spei_1","spei_2","spei_3", "spei_6", "spei_12","spei_24")
res.list[[l]] =df_drought

}
mat = matrix(nrow=length(which(gauges$bfi_class == i)), ncol=12)

for (n in 2:13)
  {
mat[,(n-1)]= sapply(1:length(which(gauges$bfi_class == i)), function(x) cor(x= res.list[[x]]$ssi,y= res.list[[x]][,n], use="na.or.complete", method = "spearman"))# spearman because we want rank correlation since ssi is nonparametric (limited to -1.97) and spi is parametric (not limited)
}

mat_cor = mat %>% as.data.frame()
colnames(mat_cor) = c("spi_01", "spi_02", "spi_03", "spi_06", "spi_12", "spi_24", "spei_01","spei_02","spei_03", "spei_06", "spei_12","spei_24")
mat_cor_long = gather(mat_cor, key=sci_type, value=cor)
mat_res[[i]] = mat_cor_long
}
return(mat_res)
}


#monthly sci analysis ####
monthly_sci = function(month=3, threshold = 0){
res.list = list()  
for (c in 1:catch_n)
  {
  
int= which(month(date_seq)==month)
df= cbind.data.frame(ssi = ssi_1[c(int),c], spi_v2_1[c(int),c], spi_v2_2[c(int),c], spi_v2_3[c(int),c], spi_v2_6[c(int),c], spi_v2_12[c(int),c], spi_v2_24[c(int),c], spei_v2_1[c(int),c], spei_v2_2[c(int),c], spei_v2_3[c(int),c], spei_v2_6[c(int),c], spei_v2_12[c(int),c], spei_v2_24[c(int),c]) 
df_drought = df %>% filter(ssi < threshold)
colnames(df_drought) = c("ssi", "spi_1", "spi_2", "spi_3", "spi_6", "spi_12", "spi_24", "spei_1","spei_2","spei_3", "spei_6", "spei_12","spei_24")
res.list[[c]] =df_drought

}
mat = matrix(nrow=catch_n, ncol=12)

for (n in 2:13)
  {
mat[,(n-1)]= sapply(1:catch_n, function(x) cor(x= res.list[[x]]$ssi,y= res.list[[x]][,n], use="na.or.complete", method = "spearman"))# spearman because we want rank correlation since ssi is nonparametric (limited to -1.97) and spi is parametric (not limited)
}
mat_cor = mat %>% as.data.frame()
colnames(mat_cor) = c("spi_1", "spi_2", "spi_3", "spi_6", "spi_12", "spi_24", "spei_1","spei_2","spei_3", "spei_6", "spei_12","spei_24")
mat_cor = cbind(mat_cor,gauge= 1:catch_n,sr=  gauges$sr_new,saar= gauges$saar, hydro_geo = gauges$hydrogeo_simple, landuse= gauges$landuse) %>% as.tbl()

mat_cor_long = gather(mat_cor, key=sci_type, value=cor, -gauge,-landuse, -sr, -saar, -hydro_geo, factor_key = TRUE) %>% as.data.frame()  
return(mat_cor_long)
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
        temp <- lm(y[,g]~x[,g], na.action = na.exclude)
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
  cat(paste0(sci_name,"_",a)," finished")
 assign(paste0(sci_name,"_",a), erg, envir = .GlobalEnv)

 }}


# drought characteristics -------------------------------------------------

#computing of drought deficit and days of drought per month per catchment per year

seasonal_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th =  function(i) {
  
temp1 = data %>%
  filter(catchment == i)
    mat_def = matrix(nrow=length(year_ta), ncol=12,data=0)
    mat_days = matrix(nrow=length(year_ta), ncol=12,data=0)

for (e in 1:max(temp1$event_no)){
#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought (in days)
  mt_yr = NULL
   mt_yr = seq.Date(from=ymd(temp1$dr_start[e]), to= ymd(temp1$dr_end[e]), by="month")
   #problem: the seq leaves out the last month if the day (in the month) of drought end is before the day (in the month) of the drought start
   if(day(ymd(temp1$dr_end[e])) <  day(ymd(temp1$dr_start[e]))){
     mt_yr = c(mt_yr, ymd(temp1$dr_end[e]))
   }
     year_y = year(mt_yr) - (min(year_ta)-1) #to set row indice in matrix
  month_x = month(mt_yr) # to set column indice in matrix

#writing to matrix
  if (length(month_x) == 1 ){#if drought starts and ends in the same month
    mat_def[cbind(year_y,month_x)] = sum(mat_def[cbind(year_y,month_x)],temp1$def_vol[e]*as.numeric(ymd(temp1$dr_end[e])-ymd(temp1$dr_start[e]))) #sum to include the information of the previous event, if for example there was a drought already at the beginning of the month
    #def.vol * number of days = total deficit of the drought event
    mat_days[year_y[1],month_x[1]] = sum(mat_days[cbind(year_y,month_x)],as.numeric(ymd(temp1$dr_end[e])-ymd(temp1$dr_start[e])))
    # drought end - drought beginning = number of days with drought 
  }else { #
  # }else{ 
  # if there was already a drought in that month the new drought event needs to be added to the drought (that already occured in that month). Happens where there are two short droughts right after each other. 
   
   if(length(month_x)>2){ 
     mat_def[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = temp1$def_vol[e]*as.numeric(days_in_month(month_x[2:(length(month_x)-1)]))
    mat_days[cbind(year_y[2:(length(year_y)-1)],month_x[2:(length(month_x)-1)])] = days_in_month(month_x[2:(length(month_x)-1)]) %>% as.numeric()
   }
    mat_def[year_y[1],month_x[1]] = sum(mat_def[year_y[1],month_x[1]],temp1$def_vol[e]*(30-day(ymd(temp1$dr_start[e]))))
    mat_def[year_y[length(year_y)],month_x[length(month_x)]] = sum(mat_def[year_y[length(year_y)],month_x[length(month_x)]],temp1$def_vol[e]*day(ymd(temp1$dr_end[e])))
    mat_days[year_y[1],month_x[1]] = sum(mat_days[year_y[1],month_x[1]],(days_in_month(month_x[1])+1)-day(ymd(temp1$dr_start[e])))#retrieving the days of in the first month of drought, +1 because the first day of the drought count's as drought
    mat_days[year_y[length(year_y)],month_x[length(month_x)]] = sum(mat_days[year_y[length(year_y)],month_x[length(month_x)]],day(ymd(temp1$dr_end[e])))
  }#
# }#
    }
   
  
return(list(mat_days, mat_def))
}

cl<-makeCluster(no_cores-1) # it is 4 times faster than the sequential loop!
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
return(res)
}

#making decadal sums of drought characteristics
seasonal_dec_80_ana = function(data= p_days_of_drought_list){
res= list()
n=1
mat = matrix(nrow=catch_n, ncol=12)
dec_years=c("1970-1980","1980-1990","1990-2000","2000-2010")
for (r in c(1,11,21,31)){
  for (i in 1:catch_n) { 
  mat[i,] = apply(data[[i]][r:(r+9),],2,sum)
  }
  res[[n]] = mat
  n=n+1
}
print({
  par(mfrow=c(2,2))
for (p in 1:4){  
 
  boxplot(as.data.frame(res[[p]]), names=c(1:12), ylim=c(0,max(res[[1]])), main= dec_years[p]) # max = res[[1]] because in the 70th there was a major drought
}
  
  })

}

#seasonal 80th data preperation for every month for trend analysis
seasonal_80th_trend = function(month = 3, datax= p_days_of_drought_list){
  res= lapply(datax, function(x) x[,month]) %>% do.call("cbind", .) %>% as.data.frame() %>% set_colnames(1:catch_n)
  return(res)
}

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


dr_count <- function(severity = -1, datax=ssi_1_long){
  try(if(severity < min(datax[,3], na.rm = T)) stop ("Too low severity. Choose higher SSI Value!!!!!"))
 res<- list()
 for (g in 1:catch_n){
s1 <- datax %>% 
  filter(gauge == g , .[,3] < severity) %>% 
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

dr_severity <- function(severity = -1, datax=ssi_1_long){
try(if(severity < min(datax[,3], na.rm=T)) stop ("Too low severity. Choose higher SSI Value!!!!!"))
res_list <- list()
raw_data = dr_count(severity = severity, datax = datax)
for (g in 1:catch_n){
    data  <- raw_data[[g]]
    res   <- matrix(nrow = max(data$event_n), ncol=6) 
for (d in 1:max(data$event_n)){
  res[d,1]        <- sum(filter(data, event_n == d) %>%  dplyr::select(3)-severity) # summing the deviation from the threshhold as the drought dsi
  res[d,2]        <- d # event number
  res[d,3]        <- data$yr_mt[data$event_n == d][1]  # drought beginning
  res[d,4]        <- tail(data$yr_mt[data$event_n == d],1) # drought end
 if(res[d,4] - res[d,3] == 0){
     res[d,5] =  days_in_month(as.Date(res[d,3], origin = "1970-01-01"))}else{
       res[d,5] =  res[d,4] - res[d,3] # drought length
     }
  res[d,6] = res[d,1]/((month(res[d,4]) - month(res[d,3]))+1) #drought intensity: dsi/drought length. basically the mean deviation from the threshold
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

sig_plot = function(p_value = .1, x_data = "mmkh_mar_mn", y_data = "mmkh_yearly_q10", output = "sr"){

  bfi_class = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=as.factor(gauges$bfi_class[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)])))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_discrete("BFI \nclass", label=c("<.4", ".4-.6", ".6-.8", ".8-1"))

  mn_t = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$mn_t[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("Mean T [°C]")
  
  
sr = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=as.factor(gauges$sr[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)])))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_discrete("Seasonality", label=c("summer", "unclear", "winter"))


hochwert = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$Hochwrt[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("Hochwert")

sr_new = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=as.factor(gauges$sr_new[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)])))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", round(p_value,3)))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_discrete("Seasonality", label=c("summer", "winter"))

saar= ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$saar[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("SAAR [mm]")

bfi = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$bfi[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("BFI")

ezgg = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$Enzgsg_[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("Einzugsgebiet [km²]")

geo =  ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=as.factor(gauges$hydrogeo_simple[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)])))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_discrete("Hydro Geo.")

mn_def =  ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$mean_deficit_overall[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("Mean deficit [m³]")

mn_def_ev = ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], x=get(x_data)$sen_slope[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)], col=gauges$mean_deficit_per_event[which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value)]))+
     annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value & get(x_data)$new_p < p_value))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", p_value))+
  xlab(paste(x_data, "sen's slope"))+
  ylab(paste(y_data, "sen's slope"))+
  scale_color_continuous("Mean deficit events [m³]")

return(get(output))
}


catch_plot = function(p_value = .1, x_data = "saar", y_data = "mmkh_yearly_q10", color = "saar", factor= TRUE){
gauges_df = gauges %>% as.data.frame()  
  if (factor == TRUE)    {
    z_value = as.factor(dplyr::select(gauges_df, color)[,1])
  }else{
    z_value = dplyr::select(gauges_df, color)[,1]
  }
  
output= ggplot()+
  geom_point( aes(y=get(y_data)$sen_slope[which(get(y_data)$new_p<p_value)], x=dplyr::select(gauges_df, x_data)[which(get(y_data)$new_p<p_value),], col=z_value[which(get(y_data)$new_p<p_value)]))+
    annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 1, label=paste("n = ", length(which(get(y_data)$new_p<p_value ))))+
  annotate(geom="text",  -Inf, Inf,  hjust = 0, vjust = 3, label=paste("p = ", round(p_value,3)))+
  xlab(x_data)+
  ylab(paste(y_data, "sen's slope"))

if(factor== TRUE){
  output = output+
      scale_color_discrete(color)#,c("Summer", "Winter"))

}else {
  output = output+
    scale_color_continuous(color)
}

return(output)
}

#from  stack over flow to give number of observations for boxplots


give.n <- function(x){
  return(c(y= -0.25, label = length(x))) #y = median(x)+.0275
}
give.n.summer <- function(x){
  return(c(y= -.25, label = round(length(x)/n_summer,2)*100)) #y = median(x)+.0275
}
give.n.winter <- function(x){
  return(c(y= -.25, label = round(length(x)/n_winter,2)*100)) #y = median(x)+.0275
}


monthly_cor_sci_yr = function(sr_x=0, sci_typex="spi"){ #calculates it for spring+summer month

mar = ggplot()+
  geom_boxplot(data=mar_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("March")+
  ylab("spearman correlation ssi-1 ~ x")+
  scale_x_discrete(labels=c(agg_month))
jun = ggplot()+
  geom_boxplot(data=jun_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y=cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("June")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))
aug = ggplot()+
  geom_boxplot(data=aug_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("August")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))
sep = ggplot()+
  geom_boxplot(data=sep_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y=cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("September")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))
  

return(grid.arrange(mar,jun,aug,sep, ncol=4))
}

monthly_cor_sci_spring = function(sr_x=0, sci_typex="spi"){ #callculates it for spring

mar = ggplot()+
  geom_boxplot(data=mar_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("March")+
  ylab("spearman correlation ssi-1 ~ x")+
  scale_x_discrete(labels=c(agg_month))
apr = ggplot()+
  geom_boxplot(data=apr_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y=cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("April")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))
mai = ggplot()+
  geom_boxplot(data=mai_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("Mai")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))
jun = ggplot()+
  geom_boxplot(data=jun_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y=cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("June")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))
  

return(grid.arrange(mar,apr,mai,jun, ncol=4))
}

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


doughnut <-
function (x, labels = names(x), edges = 200, outer.radius = 0.8, 
          inner.radius=0.6, clockwise = FALSE,
          init.angle = if (clockwise) 90 else 0, density = NULL, 
          angle = 45, col = NULL, border = FALSE, lty = NULL, 
          main = NULL, ...)
{
    if (!is.numeric(x) || any(is.na(x) | x < 0))
        stop("'x' values must be positive.")
    if (is.null(labels))
        labels <- as.character(seq_along(x))
    else labels <- as.graphicsAnnot(labels)
    x <- c(0, cumsum(x)/sum(x))
    dx <- diff(x)
    nx <- length(dx)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1L] > pin[2L])
        xlim <- (pin[1L]/pin[2L]) * xlim
    else ylim <- (pin[2L]/pin[1L]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (is.null(col))
        col <- if (is.null(density))
          palette()
        else par("fg")
    col <- rep(col, length.out = nx)
    border <- rep(border, length.out = nx)
    lty <- rep(lty, length.out = nx)
    angle <- rep(angle, length.out = nx)
    density <- rep(density, length.out = nx)
    twopi <- if (clockwise)
        -2 * pi
    else 2 * pi
    t2xy <- function(t, radius) {
        t2p <- twopi * t + init.angle * pi/180
        list(x = radius * cos(t2p), 
             y = radius * sin(t2p))
    }
    for (i in 1L:nx) {
        n <- max(2, floor(edges * dx[i]))
        P <- t2xy(seq.int(x[i], x[i + 1], length.out = n),
                  outer.radius)
        polygon(c(P$x, 0), c(P$y, 0), density = density[i], 
                angle = angle[i], border = border[i], 
                col = col[i], lty = lty[i])
        Pout <- t2xy(mean(x[i + 0:1]), outer.radius)
        lab <- as.character(labels[i])
        if (!is.na(lab) && nzchar(lab)) {
            lines(c(1, 1.05) * Pout$x, c(1, 1.05) * Pout$y)
            text(1.1 * Pout$x, 1.1 * Pout$y, labels[i], 
                 xpd = TRUE, adj = ifelse(Pout$x < 0, 1, 0), 
                 ...)
        }
        ## Add white disc          
        Pin <- t2xy(seq.int(0, 1, length.out = n*nx),
                  inner.radius)
        polygon(Pin$x, Pin$y, density = density[i], 
                angle = angle[i], border = border[i], 
                col = "white", lty = lty[i])
    }
 
    title(main = main, ...)
    invisible(NULL)
}

# trend calculaton --------------------------------------------------------
# ts_data=yearly_mean_q[,1:6]


mk_tests_par = function(raw_data =c("yearly_mean_q", "yearly_min_q","summer_ave_q","summer_min_q","summer_q_q10")){

for(d in raw_data){
  #bootstrapping
  cl = makeCluster(no_cores)
  ts_data = get(d)
  cat("calculating bootstrapped MK Trend test: " , d, "\n")
  res_bb=parSapply(cl,c(ts_data[,1:ncol(ts_data)]),FUN=bbsmK_mod )
  stopCluster(cl) #to return memory ressources to the system
  cat("finished bootstrapping starting modifed mk \n")
  #modified mk test
  res_mmkh = t(sapply(c(ts_data[,1:ncol(ts_data)]), FUN =mmkh))
  res_mmky = t(sapply(c(ts_data[,1:ncol(ts_data)]), FUN =mmky))
  assign(paste0("bb_",d ), t(res_bb), envir = .GlobalEnv)
  colnames(res_mmkh) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var")
  colnames(res_mmky) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var")
  assign(paste0("mmkh_", d), as.data.frame(res_mmkh), envir = .GlobalEnv )
  assign(paste0("mmky_", d), as.data.frame(res_mmky), envir = .GlobalEnv )
}}

#calculating man kandell trend with correction for serial correlated data using Yue and Wang's (2004) approach
mmky_par = function(raw_data =c("ms7_min", "ms30_min")){

for(d in raw_data){
  ts_data = get(d)
  #modified mk test
  res_mmky = t(sapply(c(ts_data[,1:ncol(ts_data)]), FUN =mmky_edit))
  colnames(res_mmky) = c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var","S")
  assign(paste0("mmky_", d), as.data.frame(res_mmky), envir = .GlobalEnv )
}
}


mmky_edit = function(x)
{
    x = x
    z = NULL
    z0 = NULL
    pval = NULL
    pval0 = NULL
    S = 0
    Tau = NULL
    essf = NULL
    if (is.vector(x) == FALSE) {
        stop("Input data must be a vector")
    }
    if (any(is.finite(x) == FALSE)) {
        x <- x[-c(which(is.finite(x) == FALSE))]
        warning("The input vector contains non-finite numbers. An attempt was made to remove them")
    }
    n <- length(x)
    V <- rep(NA, n * (n - 1)/2)
    k = 0
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            k = k + 1
            V[k] = (x[j] - x[i])/(j - i)
        }
    }
    slp <- median(V, na.rm = TRUE)
    t = 1:length(x)
    xn = (x[1:n]) - ((slp) * (t))
    for (i in 1:(n - 1)) {
        for (j in (i + 1):n) {
            S = S + sign(x[j] - x[i])
        }
    }
    ro <- acf(xn, lag.max = (n - 1), plot = FALSE)$acf[-1]
    rof <- rep(NA, length(ro))
    for (i in 1:(length(ro))) {
        rof[i] <- ro[i]
    }
    ess = 0
    for (k in 1:(n - 1)) {
        ess = ess + (1 - (k/n)) * rof[k]
    }
    essf = 1 + 2 * (ess)
    var.S = n * (n - 1) * (2 * n + 5) * (1/18)
    if (length(unique(x)) < n) {
        aux <- unique(x)
        for (i in 1:length(aux)) {
            tie <- length(which(x == aux[i]))
            if (tie > 1) {
                var.S = var.S - tie * (tie - 1) * (2 * tie + 
                  5) * (1/18)
            }
        }
    }
    VS = var.S * essf
    if (S == 0) {
        z = 0
        z0 = 0
    }
    if (S > 0) {
        z = (S - 1)/sqrt(VS)
        z0 = (S - 1)/sqrt(var.S)
    }
    else {
        z = (S + 1)/sqrt(VS)
        z0 = (S + 1)/sqrt(var.S)
    }
    pval = 2 * pnorm(-abs(z))
    pval0 = 2 * pnorm(-abs(z0))
    Tau = S/(0.5 * n * (n - 1))
    return(c(`Corrected Zc` = z, `new P-value` = pval, `N/N*` = essf, 
        `Original Z` = z0, `old P.value` = pval0, Tau = Tau, 
        `Sen's slope` = slp, old.variance = var.S, new.variance = VS,s_stat = S))
}

#mmky for subset####
#to see how large the influence is of the time period considered
mmky_sbst = function(raw_data =ms7_min, width = 10, start_y=1970){
  n=nrow(raw_data)
  mat = matrix(nrow=catch_n, ncol=(n-width))
  for ( i in 1:(n-width)){
  sbst = raw_data[i:(i+width-1),]
  #modified mk test
  mat[,i] = t(sapply(c(sbst[,1:ncol(sbst)]), FUN =mmky))[,7] #only interested in sen's slope sigificance is not important since the amount of data is too little to be significant
    }
  mat %<>% as.data.frame()
  colnames(mat) = c(start_y:(start_y+n-width-1))
  return(mat)
}



bbsmK_mod = function (x, ci = 0.95, nsim = 2000, eta = 1, bl.len = NULL) 
{ #altered from package modifiedmk and boot
    x = x
    ci = ci
    nsim = nsim
    eta = eta
    bl.len = bl.len
    if (is.vector(x) == FALSE) {
        stop("Input data must be a vector")
    }
    n <- length(x)
    if (n < 4) {
        stop("Input vector must contain at least four values")
    }
    if (is.null(bl.len) == FALSE) 
        if (bl.len > n) {
            stop("Block length must be less than the time series length")
        }
    if (any(is.finite(x) == FALSE)) {
        x <- x[-c(which(is.finite(x) == FALSE))]
        warning("The input vector contains non-finite numbers. An attempt was made to remove them")
    }
    if (is.null(bl.len) == TRUE) {
        bd <- qnorm((1 + ci)/2)/sqrt(n)
        ro <- acf(x, lag.max = round(n/4), plot = FALSE)$acf[-1]
        sig.v <- rep(0, round(n/4))
        for (i in 1:round(n/4)) {
            if (-bd > ro[i] | bd < ro[i]) {
                sig.v[i] <- ro[i]
            }
        }
        if (all(sig.v == 0)) {
            min.sig <- 0
        }
        else {
            min.sig.init <- rle(sig.v)
            min.sig <- max(min.sig.init$lengths[min.sig.init$values != 
                0])
        }
        bl.len <- min.sig + eta
    }
    MK.orig <- modifiedmk::mkttest(x)
    z <- round(MK.orig["Z-Value"], digits = 7)
    slp <- round(MK.orig["Sen's slope"], digits = 7)
    S <- MK.orig["S"]
    MKtau <- function(x) modifiedmk::mkttest(x)[["Tau"]]
    boot.out <- boot::tsboot(x, MKtau, R = nsim, l = bl.len, sim = "fixed", orig.t = TRUE) 
    new_tau <- mean(boot.out$t, digits = 7)
    new_se = sd(boot.out$t)
    bbs.ci <- boot::boot.ci(boot.out, conf = ci, type = "perc")$perc[4:5] 
    lb <- round(bbs.ci[1], digits = 7)
    ub <- round(bbs.ci[2], digits = 7)
    res = matrix(nrow=1, ncol=10)
    res[1,1:6] = MK.orig
    res[1,7] = new_tau
    res[1,8] = new_se
    res[1,9] = lb
    res[1,10] = ub
    res = as.data.frame(res)
    colnames(res) = c(names(MK.orig), "bb_tau","bb_se","lb", "ub")
    return(res)
}

# x=ms7_min[,229]
# mmkh = function (x, ci = 0.95)
#  {
#     x = x
#     z = NULL
#     z0 = NULL
#     pval = NULL
#     pval0 = NULL
#     S = 0
#     Tau = NULL
#     essf = NULL
#     ci = ci
#     if (is.vector(x) == FALSE) {
#         stop("Input data must be a vector")
#     }
#     if (any(is.finite(x) == FALSE)) {
#         x <- x[-c(which(is.finite(x) == FALSE))]
#         warning("The input vector contains non-finite numbers. An attempt was made to remove them")
#     }
#     n <- length(x)
#     V <- rep(NA, n * (n - 1)/2)
#     k = 0
#     for (i in 1:(n - 1)) {
#         for (j in (i + 1):n) {
#             k = k + 1
#             V[k] = (x[j] - x[i])/(j - i)
#         }
#     }
#     slp <- median(V, na.rm = TRUE)
#     t = 1:length(x)
#     xn <- (x[1:n]) - ((slp) * (t))
#     for (i in 1:(n - 1)) {
#         for (j in (i + 1):n) {
#             S = S + sign(x[j] - x[i])
#         }
#     }
#     ro <- acf(rank(xn), lag.max = (n - 1), plot = FALSE)$acf[-1]
#     sig <- qnorm((1 + ci)/2)/sqrt(n)
#     rof <- rep(NA, length(ro))
#     for (i in 1:(length(ro))) {
#         if (ro[i] > sig || ro[i] < -sig) {
#             rof[i] <- ro[i]
#         }
#         else {
#             rof[i] = 0
#         }
#     }
#     cte <- 2/(n * (n - 1) * (n - 2))
#     ess = 0
#     for (i in 1:(n - 1)) {
#         ess = ess + (n - i) * (n - i - 1) * (n - i - 2) * rof[i]
#     }
#     essf = 1 + ess * cte
#     var.S = n * (n - 1) * (2 * n + 5) * (1/18)
#     if (length(unique(x)) < n) {
#         aux <- unique(x)
#         for (i in 1:length(aux)) {
#             tie <- length(which(x == aux[i]))
#             if (tie > 1) {
#                 var.S = var.S - tie * (tie - 1) * (2 * tie +
#                   5) * (1/18)
#             }
#         }
#     }
#     VS = var.S * essf
#     if (S == 0) {
#         z = 0
#         z0 = 0
#     }
#     if (S > 0) {
#         z = (S - 1)/sqrt(VS)
#         z0 = (S - 1)/sqrt(var.S)
#     }
#     else {
#         z = (S + 1)/sqrt(VS)
#         z0 = (S + 1)/sqrt(var.S)
#     }
#     pval = 2 * pnorm(-abs(z))
#     pval0 = 2 * pnorm(-abs(z0))
#     Tau = S/(0.5 * n * (n - 1))
#     return(c(`Corrected Zc` = z, `new P-value` = pval, `N/N*` = essf,
#         `Original Z` = z0, `old P.value` = pval0, Tau = Tau,
#         `Sen's slope` = slp, old.variance = var.S, new.variance = VS))
#  }

qua_trend <- function(quantil=0.1, data_source = "q_long"){
  ken=matrix(nrow=catch_n, ncol=2)
  raw_data = get(data_source)
  data <- raw_data %>%
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

ken_trend <- function(agg_mn= c(1,2,3,6,9,12,24), data_source="spi_", sci=TRUE){
  ken_tot <- list()
  ken_tau <- data.frame()
  ken_sl <- data.frame()
  i=0
  if(sci == FALSE){
  message(paste("Ignoring the sci values since only the trend for", data_source, "is beeing calculated."))
  sci_data <- get(data_source) 
  for(g in 1:ncol(sci_data)){
  res <- MannKendall(sci_data[,g])
  ken_tau[g,1] <- res$tau
  ken_sl[g,1] <- res$sl
  ken_tot <- cbind(ken_tau[,1], ken_sl[,1])
  colnames(ken_tot) <- c("tau", "sl")
  }}else{
    for (a in agg_mn){
    i= i+1
  sci_data <- get(paste0(data_source,a))
  for(g in 1:ncol(sci_data)){
  res <- MannKendall(sci_data[,g])
  ken_tau[g,i] <- res$tau
  ken_sl[g,i] <- res$sl
  }}
 
  colnames(ken_tau) <- as.character(sci)
  colnames(ken_sl) <- as.character(sci)
  ken_tot[[1]] <- ken_tau
  ken_tot[[2]] <- ken_sl}
  return(ken_tot)
}

# extracting seasonal data ####
summer_cl = function(data_source = "mt_mn_temp", method="mean", value = "temp_m", begin =5, end=11){
 data = get(data_source)
  if(method=="mean"){
  res <- data %>% 
  filter(between(month(yr_mt), begin , end)) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(mean = get(method)(get(value)))  %>% 
  ungroup() %>% 
  spread(key=gauge, value=method) %>% 
  as.data.frame()
}else{
res <- data %>% 
  filter(month(yr_mt) >= begin &month(yr_mt)<= end) %>% 
  group_by(gauge, year(yr_mt)) %>% 
  summarise(value = get(method)(get(value))) %>% 
  ungroup() %>% 
  spread(key=gauge, value=value) %>% 
  as.data.frame()
}
 res = res[,c(1:catch_n+1)]
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

#drought attribution####
 #drought attribution correlation monthly pet and t and p ~ monthly q trends
 
 monthly.cor = function(p_y="fs_mt_med_q", p_x="fs_mn_t", cor_x= "_med_t", cor_y= "_med_q", no_sig=F){
 string_x = c()
 string_y=c()
 res=c()
 n_obs = c()
 "non_sig" = rep(10, 12)
 if(no_sig==T) {
 p_y = "non_sig"
 p_x = "non_sig"
   }
 for(i in 1:12){
   string_y[i] = paste0("mmky_",str_to_lower(month.abb[i]),cor_y) #y
   string_x[i] = paste0("mmky_",str_to_lower(month.abb[i]),cor_x) #x
   res[i]= cor(x= get(string_x[i])$sen_slope[get(string_x[i])$new_p < get(p_x)[i] & get(string_y[i])$new_p < get(p_y)[i]], y= get(string_y[i])$sen_slope[get(string_x[i])$new_p < get(p_x)[i] & get(string_y[i])$new_p < get(p_y)[i]])
   n_obs[i] = length(which(get(string_x[i])$new_p < get(p_x)[i] & get(string_y[i])$new_p < get(p_y)[i]))
 }
 res2 = cbind(res, n_obs)
 colnames(res2) = c(cor_x,paste0(cor_x, "_n"))
 return(res2)
 }

agg.meteo = function(dat=mt_sm_p_wide, fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q", subset=NULL){
  res = matrix(nrow=12, ncol=length(agg_t))
  n_obs = matrix(nrow=12, ncol=length(agg_t))
  n=1
dat_x = dat %>%
  mutate(date = ymd(date_seq))
if(is.numeric(subset)){
  dat_x = dat_x[,c(subset,catch_n+1)]
}
  for (a in agg_t){
    for ( m in 1:12){
          if(is.null(subset)){
        dat_y = get(paste0("mmky_",str_to_lower(month.abb[m]),cor_y) )
          }else{
        dat_y = get(paste0("mmky_",str_to_lower(month.abb[m]),cor_y) )[subset,]
      }
      if(a ==1) {
        meteo = dat_x %>% filter(month(date) == m) %>% 
          dplyr::select(-date) 
      }else{
        meteo = rollapply(
          data=dat_x %>% dplyr::select(-date),
          width=a,
          FUN=sum,
          by.column = TRUE,
          fill=NA,
          align="right") %>%  #rolling sum
          as.data.frame %>% 
          mutate(date = ymd(date_seq)) %>%
          filter(month(date) == m)%>%  #filtering all the month of interest
          dplyr::select(-date) 
        if(is.numeric(is.na(meteo[,1]))){
          meteo = meteo[-which(is.na(meteo[,1])),] #removing the NA produced by rollapply
        }
      }
      
        res_1_mmky = t(sapply(c(meteo[,1:ncol(meteo)]), FUN =mmky_edit)) %>% 
          set_colnames(c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var","S")) %>% 
          as.data.frame %>% 
          dplyr::select("new_p", "sen_slope")
        #print(warnings())
    res[m,n]= cor(x= res_1_mmky$sen_slope[res_1_mmky$new_p < fs & dat_y$new_p < fs], y= dat_y$sen_slope[res_1_mmky$new_p < fs & dat_y$new_p < fs], use="na.or.complete")
    
   n_obs[m,n] = length(which(res_1_mmky$new_p < fs & dat_y$new_p < fs))
   
   }
    cat(round(n/length(agg_t),2)*100,"%","\n") 
    n=n+1
    
  }
return(list(n_obs, res))
}



#statistics####
#field significance after renard 2008 and Burn et al 2002
field.significance = function(loc_sig = 0.05, data_x= ms30_min, global_sig= 0.05, nsim=600){ #burn et al worked with both levels at 0.1
  catch_nx = ncol(data_x)
  n_x=nrow(data_x)
  set.seed(1)
  loc_sig_dist = c()
  
  simulation = function(x){
  for (i in x){
  resample_mat = sapply(1:catch_nx, function(x) sample(data_x[,x], size= n_x,  replace=T))
  mkttest_res = apply(resample_mat,2,mkttest)
  loc_sig_dist = length(which(mkttest_res[5,]< loc_sig))/catch_nx # %of catchments that are significant at local significance level
  }
  return(loc_sig_dist)
  }
  
cl<-makeCluster(no_cores-1) # it is 4 times faster than the sequential loop
registerDoSNOW(cl)
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:nsim, .packages = "modifiedmk",
                        .options.snow = opts, .inorder = F,.combine = "c")%dopar%{ 
   simulation(x=c)
                        }
close(pb)
stopCluster(cl)
return(quantile(res,global_sig))
}
#statistical####
#wilcox test
wilcox.test.modified = function(x_m="mmky_wi_mn_t", y_m= NULL, fs_x=NULL, fs_y = NULL){
  
  if(is.null(y_m)) y_m=get(x_m)
  if(is.null(fs_x)){
  fs_x= get(paste0("fs_",substr(x_m,6, str_count(x_m))))
  }
  if(is.null(fs_y)) fs_y = fs_x
  wilcox.test(x = get(x_m)$sen_slope[get(x_m)$new_p < fs_x  & gauges$sr_new == 0], y= y_m$sen_slope[y_m$new_p < fs_y& gauges$sr_new == 2])

}

#sens slope error bars
# after Gilbert 1987

error.bar = function(data= "mar_mn_q"){
  dat = get(data)
  sen_dat= get(paste0("mmky_", data))
  res = apply(dat, 2, EnvStats::kendallTrendTest) # calculating the sens slope confidence intervals with the EnvStats package; default is exactly what I want to calculate
  low_lim = sapply(1:catch_n, function(x) res[[x]]$interval$limits[1]) #extracting the lower limit
  upp_lim = sapply(1:catch_n, function(x) res[[x]]$interval$limits[2]) # upper limit
  # assign(x= paste0("error_",data), cbind.data.frame(low_lim, upp_lim  , sen_dat=  sen_dat$sen_slope), envir = .GlobalEnv)
  return(cbind.data.frame(low_lim, upp_lim  , sen_dat=  sen_dat$sen_slope))
}

