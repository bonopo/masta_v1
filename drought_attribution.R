
# drought attribution -----------------------------------------------------

#cluster analysis & PCA ####
#problem: too many response variables that are highly corrlinated since they are calculated from the same data set. picking only the ones that are not correlated.

clust_ana = cbind(mmkh_ms7_min$sen_slope, mmkh_ms7_date$sen_slope, mmkh_ms30_min$sen_slope, mmkh_su_q10$sen_slope, mmkh_wi_q10$sen_slope, mmkh_yearly_mn_q$sen_slope, mmkh_yearly_q10$sen_slope, mmkh_mar_mn_q$sen_slope, mmkh_jun_mn_q$sen_slope) %>% as.data.frame()
colnames(clust_ana)= c( "ms7_date", "ms7_min", "ms30_min", "su_q10", "wi_q10","yearly_mn_q","yearly_q10","mar_mn_q","jun_mn_q")

clust_ana2 = cbind(mmkh_su_mn_t$sen_slope, mmkh_wi_mn_t$sen_slope,mmkh_yearly_mn_t$sen_slope, mmkh_yearly_max_t$sen_slope, mmkh_yearly_sm_p$sen_slope, mmkh_su_sm_p$sen_slope, mmkh_wi_sm_p$sen_slope, mmkh_mar_mn_t$sen_slope, mmkh_jun_mn_t$sen_slope)%>% as.data.frame()

colnames(clust_ana2)= c( "su_mn_t", "wi_mn_t","yearly_mn_t", "yearly_max_t", "yearly_sm_p",    "su_sm_p", "wi_sm_p","mar_mn_t","jun_mn_t")

png("./plots/further_investigate/final/cluster_response.png", width=1000, height=500)
library(scales)
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
gauges$best_spi_n = apply(cor_spi, 1, which.max)
gauges$best_spi_cor =apply(cor_spi, 1, max)
gauges$best_spei_n = apply(cor_spei, 1, which.max)
gauges$best_spei_cor =apply(cor_spei, 1, max)



plot(best_spi_cor)
hist(best_spi_n)
boxplot(best_spi_n~best_spei_n)
#with regression of residuals of ssi-1 ~ spi-n####



hist(drought_sci[[3]]$spi)
plot(y= drought_sci[[3]]$ssi , x= drought_sci[[3]]$spi)
t = na.omit(drought_sci[[3]]$spi)
fd = fitdistr(t, "normal")
logLik(fd)
AIC(fd)


glm_sum= lapply(1:catch_n, function (x) summary(lm(ssi ~ spi , data= drought_sci[[gauges$best_spi_n[x]]] %>%  filter(gauge==x))))
glm_resid = lapply(1:catch_n, function (x) residuals(lm(ssi ~ spi , data= drought_sci[[gauges$best_spi_n[x]]] %>%  filter(gauge==x),na.action=na.exclude))) #very important if not the na are thrown out and this causes different length in the residuals. Making resials shorter than the predictor variable. The NA occur when the SPI value is NA, this happens for all aggregation month > 1 in the beginning of the time series. E.g. for SPI-24 all droughts happening in the year 1970 and 1971 (minus december) have an NA value as SPI since it needs 24 month to aggregate. 

p=c()
for (i in 1:338){

  p[i]=length(drought_sci[[1]]$ssi[drought_sci[[1]]$gauge == i])
  #Sys.sleep(.1)
}
# 
# 

#resid = do.call("cbind", glm_resid) doesn't work since #some catchments have 73 droughts in 40 years some 72 (due to the nonparametric approach)
spei_expl 

resid_sig = do.call("cbind", glm_resid)[,spei_expl]
 colnames(resid_sig) =c(spei_expl)

hist(resid_sig[,1])# analysis of redisuals, they are rel. normal distributed

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
 plot(glm_resid_final[[]]$residuals)# ~ best_spei, data = glm_resid_final[[i]])
#   Sys.sleep(.1)
   p_value[i,]= glm_spei_resid[[i]]$coefficients[,4]  
 
}
spei_expl = which(p_value[,1]<.1& p_value[,2]<.1)


for(i in spei_expl){

plot(glm_resid_final[[i]]$residuals )
 # Sys.sleep(.1)

 
}

#speichertransfer ####

hist(gauges$bfi) # normal
hist(mmky_mar_mn_q$sen_slope) #not normal
hist(mmky_jun_mn_q$sen_slope)

y = mmky_jun_mn_q$sen_slope[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)]
x= mmky_mar_mn_q$sen_slope[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)]
x2 = gauges$sr[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)]
data_plot = cbind.data.frame(y, x, x2)
#problem one variable is normal the other is positivly skewed
fm = lm(y ~ x*x2)
summary(fm)
logLik(fm)
str(fm)
summary(fm)$adj.r.squared
#with negative skewnesss package sn
fm_sn = selm(y~x*x2, family= "SN", data=data_plot)
summary(fm_sn) #higher log likelihood therefore better
# new_data = predict.selm(fm_sn, newdata = data_plot$x, param.type = "cp", interval = "prediction" ) # With the "CP" option (that is, the 'centred
#parametrization'), the residuals are centred around 0, at least approximately; 
predict(fm_sn)


data_plot = cbind.data.frame(y, x, x2)
ggplot(data= data_plot, aes(y=y, x=x, col=as.factor(x2)))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  annotate(geom="text", -Inf, Inf,  hjust = -0.2, vjust = 2, label=paste("n = ", length(y)))+
  annotate(geom="text", -Inf, Inf,  hjust = -0.2, vjust = 4, label=paste("p = 0.05"))+
  annotate(geom="text", -Inf, Inf,  hjust =-0.2, vjust = 6, label=paste("r²=",round(summary(fm)$adj.r.squared,2)))+
  xlab(paste("mmky March mean q sen's slope"))+
  ylab(paste("mmky June mean q sen's slope"))+
  scale_color_discrete("Seasonality", label=c("summer", "unclear", "winter"))
#this is linear model with normal distribution not with selm and skewed normal distr

ggsave("./plots/further_investigate/final/lm_sr_wi_su_q10.png")

hist(mmky_su_q10$sen_slope) #normal
hist(mmky_wi_q10$sen_slope) #normal
hist(gauges$sr)

y= mmky_wi_q10$sen_slope[which(mmky_wi_q10$new_p<.05 & mmky_su_q10$new_p<.05)]
x= mmky_su_q10$sen_slope[which(mmky_wi_q10$new_p<.05 & mmky_su_q10$new_p<.05)]
x2 =gauges$sr[which(mmky_wi_q10$new_p<.05 & mmky_su_q10$new_p<.05)]

fm = lm(y ~ x *x2 )
summary(fm)
# fm_sn = selm(y~x*x2, family= "SN")
# summary(fm_sn)
data_plot = cbind.data.frame(y, x, x2)
ggplot(data= data_plot, aes(y=y, x=x, col=as.factor(x2)))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  annotate(geom="text", Inf, Inf,  hjust = 1, vjust = 2, label=paste("n = ", length(y)))+
  annotate(geom="text", Inf, Inf,  hjust = 1, vjust = 4, label=paste("p = 0.05"))+
  annotate(geom="text", Inf, Inf,  hjust =1, vjust = 6, label=paste("r²=",round(summary(fm)$adj.r.squared,2)))+
  xlab(paste("mmky Summer q10 sen's slope"))+
  ylab(paste("mmky Wintzer q10 sen's slope"))+
  scale_color_discrete("Seasonality", label=c("summer", "unclear", "winter"))

#stepwise regression ####
mmky_ms7_min$sen_slope %>% hist()
y = mmky_ms7_min$sen_slope

step_fm=step(lm(mmky_ms7_min$sen_slope ~ mmky_jun_mn_q$sen_slope + mmky_mar_mn_q$sen_slope+ mmky_wi_mn_t$sen_slope+mmky_su_mn_t$sen_slope + mmky_yearly_max_t$sen_slope + mmky_su_mn_t$sen_slope + mmky_wi_mn_t$sen_slope ), direction = "both" , k=log(catch_n))

lm(mmky_ms7_min$sen_slope ~ mmky_jun_mn_q$sen_slope + mmky_mar_mn_q$sen_slope+ mmky_wi_mn_t$sen_slope+mmky_su_mn_t$sen_slope + mmky_yearly_max_t$sen_slope + mmky_su_mn_t$sen_slope + mmky_wi_mn_t$sen_slope ) %>% summary()

summary(lm(mmky_ms7_min$sen_slope ~ mmky_jun_mn_q$sen_slope + mmky_mar_mn_q$sen_slope+ mmky_wi_mn_t$sen_slope+mmky_su_mn_t$sen_slope + mmky_yearly_max_t$sen_slope + mmky_su_sm_p$sen_slope + mmky_wi_sm_p$sen_slope))

#*gauges$landuse & gauges$Enzgsg_ contain NAs

dat = cbind.data.frame(y=mmky_su_sm_p$sen_slope, sr=gauges$sr,saar=gauges$saar,hydrogeo= gauges$hydrogeo_simple,q_mean=gauges$q_mean,su_mn_t=gauges$su_mn_t,wi_mn_t=gauges$wi_mn_t,wi_sm_p= gauges$wi_sm_p,su_sm_p= gauges$su_sm_p,best_spi_n=gauges$best_spi_n,best_spi_cor=gauges$best_spi_cor)

fm= step(lm(y~ mmky_su_sm_p$sen_slope* gauges$sr*gauges$saar*gauges$hydrogeo_simple*gauges$q_mean*gauges$su_mn_t*gauges$wi_mn_t*gauges$wi_sm_p*gauges$su_sm_p*gauges$best_spi_n*gauges$best_spi_cor), direction = "both" , k=log(catch_n))
fm= lm(y~ mmky_su_sm_p$sen_slope* gauges$sr*gauges$saar*gauges$hydrogeo_simple*gauges$q_mean*gauges$su_mn_t*gauges$wi_mn_t*gauges$wi_sm_p*gauges$su_sm_p*gauges$best_spi_n*gauges$best_spi_cor)
summary(min.model)
min.model = lm(y~1,data=dat )
#gauges$saar removed
fwd.model = step(min.model, direction = "both", scope = (~ gauges$sr*gauges$hydrogeo_simple*gauges$q_mean*gauges$su_mn_t*gauges$wi_mn_t*gauges$wi_sm_p*gauges$su_sm_p*gauges$best_spi_n*gauges$best_spi_cor))

summary(fwd.model)
AIC(fwd.model)
head(iris)

stepAIC(fm, direction = "backward", k=log(catch_n))

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

# regression with best_spi_n ####

fm=lm(gauges$best_spi_n ~ gauges$hydrogeo_simple + gauges$bfi)
summary(fm)

ggplot()+
  geom_point(aes(y=as.factor(gauges$best_spi_n), col=gauges$best_spi_cor, x=gauges$saar))


+
  xlab("Peason correlation")+
  scale_color_discrete("Hydrogeology", labels=c("Chasm", "others", "Pores"))
  scale_y_discrete("best spi-n",labels= c("1"="1", "2"="2", "3"="3", "4"="6", "5"="12", "6"="24"))
  ggsave("./plots/further_investigate/final/best_spi_cor.png")
  

#+
  geom_smooth(method="lm", se = TRUE)+
  annotate(geom="text", Inf, Inf,  hjust = 1, vjust = 2, label=paste("n = ", length(y)))+
  annotate(geom="text", Inf, Inf,  hjust = 1, vjust = 4, label=paste("p = 0.05"))+
  annotate(geom="text", Inf, Inf,  hjust =1, vjust = 6, label=paste("r²=",round(summary(fm)$adj.r.squared,2)))+
  xlab(paste("mmky Summer q10 sen's slope"))+
  ylab(paste("mmky Wintzer q10 sen's slope"))+
  scale_color_discrete("Seasonality", label=c("summer", "unclear", "winter"))

