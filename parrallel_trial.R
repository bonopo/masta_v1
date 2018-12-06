setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
load("./output/drought_q.Rdata", verbose = TRUE)
drought_q= output
catch_n =338


par_seas_80th = function(data= drought_q ){
no_cores=detectCores()
cl<-makeCluster(no_cores-1) #change the 2 to your number of CPU cores
registerDoSNOW(cl)
res=list()
def_catch = c()
mean_def=c()
mean_n = c()

pb <- txtProgressBar(max = catch_n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)
opts <- list(progress = progress)

res <- foreach::foreach(.packages = c("tidyverse", "lubridate", "foreach"), 
                        #.options.snow = opts,.combine = cbind, .inorder = FALSE,
                        chunk = isplitVector(1:catch_n, chunks=getDoParWorkers()))%dopar%{ ###cbind
                          foreach(c=chunk) %do%{
    
    temp1 = data %>%
    filter(catchment == c)
for (m in 1:12){
  def_catch=NULL
 

 for (e in 1:max(temp1$event_no)){
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
   def_catch = rbind(def_catch,temp1$def_vol[e]*(dr_len/length(months))) #calculating the deficit of all drought events of one catchment in one particular month (m) and rbinding them
 }else{
   next
 }
 }  

mean_def[m] = round(mean(def_catch),0)
mean_n[m] = length(def_catch)



}
cbind(mat_def, mat_n)
                          }
                        }
close(pb)
stopCluster(cl)

mean_def_list <- lapply(res, function(x) x[,1])
mean_def_df = do.call( "cbind",mean_def_list) %>% as.data.frame() %>% set_colnames(1:catch_n)
}
