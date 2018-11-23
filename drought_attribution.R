
# drought attribution -----------------------------------------------------

#cluster analysis & PCA ####
#problem: too many response variables that are highly corrlinated since they are calculated from the same data set. picking only the ones that are not correlated.

clust_ana = cbind(mmkh_ms7_min$sen_slope, mmkh_ms7_date$sen_slope, mmkh_ms30_min$sen_slope, mmkh_su_q10$sen_slope, mmkh_wi_q10$sen_slope, mmkh_yearly_mn_q$sen_slope, mmkh_yearly_q10$sen_slope, mmkh_mar_mn_q$sen_slope, mmkh_jun_mn_q$sen_slope) %>% as.data.frame()
colnames(clust_ana)= c( "ms7_date", "ms7_min", "ms30_min", "su_q10", "wi_q10","yearly_mn_q","yearly_q10","mar_mn_q","jun_mn_q")

clust_ana2 = cbind(mmkh_su_mn_t$sen_slope, mmkh_wi_mn_t$sen_slope,mmkh_yearly_mn_t$sen_slope, mmkh_yearly_max_t$sen_slope, mmkh_yearly_sm_p$sen_slope, mmkh_su_sm_p$sen_slope, mmkh_wi_sm_p$sen_slope, mmkh_mar_mn_t$sen_slope, mmkh_jun_mn_t$sen_slope)%>% as.data.frame()

colnames(clust_ana2)= c( "su_mn_t", "wi_mn_t","yearly_mn_t", "yearly_max_t", "yearly_sm_p",    "su_sm_p", "wi_sm_p","mar_mn_t","jun_mn_t")

png("./plots/further_investigate/final/cluster_response.png", width=1000, height=500)
plot(Hmisc::varclus(~., data=clust_ana2), las=1, cex.lab=1.5)
abline(h=.5, lty=2)
dev.off()

pca <- prcomp(clust_ana2, scale=T)
summary(pca)
screeplot(pca)
biplot(pca)

devtools::install_github("sinhrks/ggfortify")
library(ggfortify)
ggplot2::autoplot(stats::prcomp(clust_ana2, scale=TRUE), label = FALSE, loadings.label = TRUE)

remove(clust_ana)
#collinearity ####

round(cor(clust_ana2),2)
#rule of thumb .7 (dorman)

#variance inflation factor ####
fm = glm(mmkh_ms7_min$sen_slope ~ ., data=clust_ana2)
summary(fm)
car::vif(fm)
#schwellenwert von 10 (dorman)
#problem with yearly_mn_t, yearly_sm_p and su_mn_t, wi_mn_t
#yearly_mn_t with su_mn_t and wi_mn_t
#yearly_sm_p with su_sm_p and wi_sm_p
# thow out yearly_mn_t and yearly_sm_p

#correlating discharge drought periods with climate indicators (SCI)####

dr_corr = function(threshhold = -1,  agg_month = c(1,2,3,6,12,24)){
  
  result_part= matrix(nrow = 0, ncol=6)
  result = list()
  

droughts_q =ssi_1_long %>% 
  filter(ssi<=threshhold) %>% 
  as.data.frame()

spi_d = lapply(agg_month, FUN = function(x)  cbind(get(paste0("spi_v2_", x)), ymd(date_seq)) %>% as.data.frame() %>% gather(key=gauge, value=spi,-`ymd(date_seq)`))

spei_d = lapply(agg_month, FUN = function(x)  cbind(get(paste0("spei_v2_", x)), ymd(date_seq)) %>% as.data.frame() %>% gather(key=gauge, value=spei,-`ymd(date_seq)`))


for(i in 1:length(agg_month)){ #for every aggregation month 
  for (g in 1:catch_n){ #for every catchment
    
    spi_d_catch = spi_d[[i]] %>% 
        filter(gauge == g)
   
    spei_d_catch = spei_d[[i]] %>% 
        filter(gauge == g)
    
    droughts_q_catch = droughts_q %>% #result per catchment
      filter(gauge==g)
    

  int= pmatch(droughts_q_catch$yr_mt,spei_d_catch$`ymd(date_seq)`) #getting the value of spi or spei of every month with drought (discharge) 

  
  # time= Sys.time()
  #   int_spei= lapply(1:10, FUN= function(x) pmatch(droughts_q[droughts_q$gauge==x,1],spei_d[[i]]$`ymd(date_seq)` ))
  #   end= Sys.time()-time # for loop is faster!!
  
   
# 
#   install.packages("lm4")
#   lm4::
#   time= Sys.time()
#    i=1
#    vec = 1:10
#   cl = makeCluster(no_cores)
#   x= c(1:10)
#   clusterExport(cl=cl, "x")
#   res = clusterEvalQ(cl=cl, function(x)
#         pmatch(droughts_q[droughts_q$gauge==x,1],spei_d[[1]]$`ymd(date_seq)`))
#   
#   resPp= parallel::parLapply(cl=cl,1:10,droughts_q, fun= f)
#   
#   
#                              fun= function(x)
#         pmatch(droughts_q[droughts_q$gauge==x,1],spei_d[[1]]$`ymd(date_seq)`, envir = .GlobalEnv))
#    stopCluster(cl) #to return memory ressources to the system
#      Sys.time()-time # for loop is faster!!
#      clusterEvalQ(cl=cl, function(x) pmatch(droughts_q[droughts_q$gauge==x,1],spei_d[[1]]$`ymd(date_seq)`))
# # 
# #      
#      f= function(x) {
#         pmatch(droughts_q[droughts_q$gauge==x,1],spei_d[[i]]$`ymd(date_seq)`)
#        }
# #      
# # clusterApply(cl, 1:2, get("+"), 3)


  
catch_res = droughts_q_catch %>%  
    mutate(spi= spi_d_catch$spi[int], spei= spei_d_catch$spei[int], date_sci= spei_d_catch$`ymd(date_seq)`[int])
  
  result_part = rbind(result_part, catch_res)

  
  }
  result[[i]] = result_part
  result_part= matrix(nrow = 0, ncol=6)
  cat(round(i/(length(agg_month)),2)*100,"%" ,"\n")

  }
return(result)
}

drought_sci = dr_corr(threshhold = -1)

#correlation of ssi-1 with spi-/spei-n in drought periods####

cor_spi = matrix(nrow=catch_n, ncol=length(drought_sci))
cor_spei = matrix(nrow=catch_n, ncol=length(drought_sci))

for (a in 1:length(drought_sci)){
for (g in 1:catch_n){
temp= drought_sci[[a]] %>% 
  filter(gauge== g)
cor_spi[g, a] = cor(y= temp$ssi , x= temp$spi, use="c", method = "p") 
cor_spei[g, a] = cor(y= temp$ssi , x= temp$spei, use="c", method = "p")
}
}
#plot(y= drought_sci[[3]]$ssi , x= drought_sci[[3]]$spi_3)


boxplot(cor_spi, names=agg_month, xlab="SPI-n", ylab="pearson correlation")
#negative trend??? bad correlation
#which aggregation month has the highest cor####
best_spi_n = apply(cor_spi, 1, which.max)
best_spi_cor =apply(cor_spi, 1, max)
best_spei_n = apply(cor_spei, 1, which.max)
best_spei_cor =apply(cor_spei, 1, max)
plot(best_spi_cor)
boxplot(best_spi_n~best_spei_n)
#drought attribution to temperature####

#with regression of residuals of ssi-1 ~ spi-n####



hist(drought_sci[[3]]$spi)
plot(y= drought_sci[[3]]$ssi , x= drought_sci[[3]]$spi)
t = na.omit(drought_sci[[3]]$spi)
fd = fitdistr(t, "normal")
logLik(fd)
AIC(fd)


glm_sum= lapply(1:catch_n, function (x) summary(lm(ssi ~ spi , data= drought_sci[[best_spi_n[x]]] %>%  filter(gauge==x))))
glm_resid = lapply(1:catch_n, function (x) residuals(lm(ssi ~ spi , data= drought_sci[[best_spi_n[x]]] %>%  filter(gauge==x),na.action=na.exclude))) #very important if not the na are thrown out and this causes different length in the residuals. Making resials shorter than the predictor variable. The NA occor when the SPI value is NA, this happens vor all aggregation month > 1 in the beginning of the time series. E.g. for SPI-24 all droughts happening in the year 1970 and 1971 (but december) have an NA value as SPI since it needs 24 month to aggregate. 

p=c()
for (i in 1:338){

  p[i]=length(drought_sci[[1]]$ssi[drought_sci[[1]]$gauge == i])
  #Sys.sleep(.1)
}
# 
# 

#resid = do.call("cbind", glm_resid) doesn't work since #some catchments have 73 droughts in 40 years some 72 (due to the nonparametric approach)

glm_resid[[2]][,1] %>% hist()# analysis of redisuals, they are rel. normal distributed

glm_resid_final = list()
for (i in 1:length(glm_resid)){
  drought_sci_temp = drought_sci[[best_spei_n[i]]] %>% 
      filter(gauge== i)
  
  glm_resid_final[[i]] = cbind.data.frame(residuals = glm_resid[[i]],best_spei = drought_sci_temp$spei) 
  # taking the spei-n (aggregation months) with the highest correlation
}

class(  glm_resid_final[[i]])
glm_spei_resid = lapply(1:catch_n, function (x) summary(lm(residuals ~ best_spei, data = glm_resid_final[[x]])))

p_value=matrix(nrow=catch_n, ncol = 2)
for(i in 1:catch_n){
# plot(residuals ~ best_spei, data = glm_resid_final[[i]])
#   Sys.sleep(.1)
   p_value[i,]= glm_spei_resid[[i]]$coefficients[,4]  
 
}
spei_expl = which(p_value[,1]<.1& p_value[,2]<.1)


for(i in spei_expl){

plot(residuals ~ best_spei, data = glm_resid_final[[i]])
 # Sys.sleep(.1)

 
}
