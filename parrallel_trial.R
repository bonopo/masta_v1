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
