#parrallel version (not working)  monthly aggregation cor
agg.meteo_par = function(dat=mt_sm_p_wide, fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q"){
  res = matrix(nrow=12, ncol=length(agg_t))
  n_obs = matrix(nrow=12, ncol=length(agg_t))
  n=1
dat_x = dat %>%
  mutate(date = ymd(date_seq))

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


simulation = function(x){
  for (a in x){
    for ( m in 1:12){
      string_y = paste0("mmky_",str_to_lower(month.abb[m]),cor_y) 
      if(a ==1) 
        {
        meteo = dat_x %>% filter(month(date) == m) %>% 
          dplyr::select(-date) 
      }else{
        meteo = rollapply(
          data=dat,
          width=a,
          FUN=sum,
          by.column = TRUE,
          fill=NA,
          align="right") %>%  #rolling sum
          as.data.frame %>% 
          mutate(date = ymd(date_seq)) %>%
          filter(month(date) == m)%>%  #filtering all the month of interest
          dplyr::select(-date) 
        meteo = meteo[-c(1:a),] #removing the NA preoduced by rollapply
      }
      
        res_1_mmky = t(sapply(c(meteo[,1:ncol(meteo)]), FUN =mmky_edit)) %>% 
          set_colnames(c("corrected_z","new_p","n/n*", "orig_z", "old_p", "tau", "sen_slope", "old_var", "new_var","S")) %>% 
          as.data.frame %>% 
          dplyr::select("new_p", "sen_slope")
        
    res[m,n]= cor(x= res_1_mmky$sen_slope[res_1_mmky$new_p < fs & get(string_y, envir = .GlobalEnv)$new_p < fs], y= get(string_y, envir = .GlobalEnv)$sen_slope[res_1_mmky$new_p < fs & get(string_y, envir = .GlobalEnv)$new_p < fs])
    
   n_obs[m,n] = length(which(res_1_mmky$new_p < fs & get(string_y, envir = .GlobalEnv)$new_p < fs))
    }
    n=n+1
   
  }
return(list(n_obs, res))
}
cl<-makeCluster(no_cores-1) # it is 4 times faster than the sequential loop
registerDoSNOW(cl)
pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
agg_res<- foreach::foreach(c = agg_t,
                           .packages = c("zoo","modifiedmk", "stringr","tidyverse", "lubridate","magrittr"),
                        .options.snow = opts, .inorder = T,.combine = "c")%dopar%{ 
  simulation(x=c)
                        }
close(pb)
stopCluster(cl)
return(agg_res)
}





#####
no_cores=detectCores()
cl<-makeCluster(no_cores-1) 
registerDoSNOW(cl)
res=list()


pb <- txtProgressBar(max = 10, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)
res <- foreach::foreach(c = 1:10, .packages = c("tidyverse", "lubridate"), 
                        .options.snow = opts)%dopar%{ ###cbind
 

   sub_80th(e=c)


                        }
close(pb)
stopCluster(cl)

return(res)
}
Sys.time()-stime


return(list(mean_def_df,mean_n_df, sm_days_df))
# return(res)


}

sub_80th =  function(e) {
  def_catch = c()
  mean_def=c()
  mean_n = c()
  sm_length = c()
  
    temp1 = data %>%
    filter(catchment == e)
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
return(cbind(mean_def, mean_n,sm_length))
}


seasonal_80th = function(data= drought_q, year_ta= 1970:2009){
sub_80th =  function(i) {
  
#  for ( i in 1:338){#
temp1 = data %>%
  filter(catchment == i)
    mat_def = matrix(nrow=length(year_ta), ncol=12)
    mat_days = matrix(nrow=length(year_ta), ncol=12)

for (e in 1:max(temp1$event_no)){
#retrieving length of drought. since def.vol is in m³/day it has to be multiplied by the length of the drought (in days)
  mt_yr = NULL
   mt_yr = seq.Date(from=ymd(temp1$dr_start[e]), to= ymd(temp1$dr_end[e]), by="month")
   if(month(ymd(temp1$dr_start[e])) !=  month(ymd(temp1$dr_end[e]))){
     mt_yr = c(mt_yr, ymd(temp1$dr_end[e]))
   }
   
  year_y = year(mt_yr) - (min(year_ta)-1)
  month_x = month(mt_yr)
if(is.null(mt_yr)) 
  {break()}
#writing to matrix
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
  
  
  
}
  
cat( i)}#

return(list(mat_days, mat_def))

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

# sum_def_list <- lapply(res, function(x) x[,1])
# sum_def_df = do.call( "cbind",sum_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
# mean_n_list <- lapply(res, function(x) x[,2])
# mean_n_df = do.call( "cbind",mean_n_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
# sm_days_list <- lapply(res, function(x) x[,3])
# sm_days_df = do.call( "cbind",sm_days_list) %>% as.data.frame() %>% set_colnames(1:catch_n)

return(res)
}

