
# drought attribution -----------------------------------------------------

#cluster analysis & PCA ####
#problem: too many response variables that are highly corrlinated since they are calculated from the same data set. picking only the ones that are not correlated.

clust_ana = cbind(mmky_ms7_min$sen_slope, mmky_ms7_date$sen_slope, mmky_ms30_min$sen_slope, mmky_su_q10$sen_slope, mmky_wi_q10$sen_slope, mmky_yearly_mn_q$sen_slope, mmky_yearly_q10$sen_slope, mmky_mar_mn_q$sen_slope, mmky_jun_mn_q$sen_slope) %>% as.data.frame()
colnames(clust_ana)= c( "ms7_date", "ms7_min", "ms30_min", "su_q10", "wi_q10","yearly_mn_q","yearly_q10","mar_mn_q","jun_mn_q")

clust_ana2 = cbind(mmky_su_mn_t$sen_slope, mmky_wi_mn_t$sen_slope,mmky_yearly_mn_t$sen_slope, mmky_yearly_max_t$sen_slope, mmky_yearly_sm_p$sen_slope, mmky_su_sm_p$sen_slope, mmky_wi_sm_p$sen_slope, mmky_mar_mn_t$sen_slope, mmky_jun_mn_t$sen_slope)%>% as.data.frame()

colnames(clust_ana2)= c( "su_mn_t", "wi_mn_t","yearly_mn_t", "yearly_max_t", "yearly_sm_p",    "su_sm_p", "wi_sm_p","mar_mn_t","jun_mn_t")

clust_ana3 = cbind( gauges$alpine, gauges$sr_new, as.factor(gauges$hydrogeo_simple), as.factor(gauges$landuse), gauges$mn_q, gauges$saar, gauges$bfi, gauges$Enzgsg_, gauges$mn_t, gauges$lt_memoryeffect,gauges$mn_deficit, gauges$mn_intensity, gauges$mn_length, gauges$cor_spei, gauges$cor_spi, gauges$cor_spei_n, gauges$cor_spi_n, gauges$Hochwrt ) %>% as.data.frame()

colnames(clust_ana3) =c("alpine",  "seasonality","hydro_geo","landuse","q_mean","saar","BFI","catchment_km", "mn_t", "memory_effect", "mn_deficit","mn_intensity","mn_length","cor_spei","cor_spi","cor_spei_n","cor_spi_n", "hochwert")
library(scales)
data_clus = cbind(clust_ana2, clust_ana3)#[,-c(3,4)]

pdf("./plots/3_choice/cluster_response.pdf" )
plot(Hmisc::varclus(~., data=clust_ana3), las=1, cex.lab=1.5)
abline(h=.5, lty=2)
dev.off()
options(na.action)
pca <- prcomp(cbind(clust_ana2, clust_ana3), scale=T, na.action= na.delete)
summary(pca)
screeplot(pca)
biplot(pca)

devtools::install_github("sinhrks/ggfortify")
library(ggfortify)
ggplot2::autoplot(stats::prcomp(clust_ana2, scale=TRUE), label = FALSE, loadings.label = TRUE)

remove(clust_ana)
#collinearity ####

cor_mat = round(cor(data_clus, use="na.or.complete", method="p"),2)
xtable::xtable(cor_mat)
#rule of thumb .7 (dorman)


#variance inflation factor ####
fm = glm(mmky_ms7_min$sen_slope ~ ., data=cbind(clust_ana2, clust_ana3))
summary(fm)
car::vif(fm)
#schwellenwert von 10 (dorman) or 5 bachmair et al 2018
#problem with yearly_mn_t, yearly_sm_p and su_mn_t, wi_mn_t
#yearly_mn_t with su_mn_t and wi_mn_t
#yearly_sm_p with su_sm_p and wi_sm_p
# thow out yearly_mn_t and yearly_sm_p




which(gauges$cor_spi_dr > .5) 





# png("./plots/3_choice/bxplt_dr_spi_1_spear.png", width=1000, height=500)
# par(mfrow=c(1,2))
# boxplot(cor_spi, names=agg_month, xlab="SPI-n", ylab="spearman correlation with SSI-1 (<-1)", ylim=c(-.4,.8))
# boxplot(cor_spei, names=agg_month, xlab="SPEI-n", ylab="spearman correlation with SSI-1 (<-1)", ylim=c(-.4,.8))
# dev.off()

colnames(cor_spi) = c("spi_1", "spi_2", "spi_3", "spi_6", "spi_12", "spi_24")
colnames(cor_spei) = c("spei_1"," spei_2"," spei_3", "spei_6", "spei_12"," spei_24")



int= which(cor_spi[,4] > .5)
gauges$bfi[int]
gauges$Hochwrt[int]

gauges$cor_spi_6 = cor_spi[,4]

spplot(gauges, "cor_spi_6")


cor_spi_long = cor_spi %>% 
  as.data.frame() %>% 
  mutate(sr = gauges$sr_new, hydro_geo = gauges$hydrogeo_simple, landuse= gauges$landuse, bfi= gauges$bfi, saar = gauges$saar, gauge= 1:catch_n) %>% 
  gather(key=spi_type, value= spear_cor,  factor_key = T, -saar, -sr, -landuse, -hydro_geo, -bfi, -gauge) %>% 
  as.tbl() 

cor_spei_long = cor_spei %>% 
  as.data.frame() %>% 
  mutate(sr = gauges$sr_new, hydro_geo = gauges$hydrogeo_simple, landuse= gauges$landuse, bfi= gauges$bfi, saar = gauges$saar, gauge= 1:catch_n) %>% 
  gather(key=spei_type, value= spear_cor,  factor_key = T, -saar, -sr, -landuse, -hydro_geo, -bfi, -gauge) %>% 
  as.tbl() 

winter = ggplot()+
  geom_boxplot(data=cor_spi_long %>% filter(spi_type == "spi_3", sr==2), aes(x=saar, y = spear_cor)) +
  ylim(c(0,.75))+
  xlab("Winter low flows")+
  ylab("spearman correlation ssi-1 ~ x (<-1)")
summer = ggplot()+
  geom_boxplot(data=cor_spi_long %>% filter(spi_type == "spi_3", sr==0), aes(x=saar, y=spear_cor))+
  ylim(c(0,.75))+
  xlab("Summer low flows")+
  ylab("spearman correlation ssi-1 ~ x (<-1) ")
grid.arrange(winter, summer, ncol=2)
             
ggplot(data= cor_spi_long %>% filter(spi_type == "spi_3"))+
  geom_point(aes(col=as.factor(sr), y= spear_cor, x=landuse))

cor_jun_mar_spei= sapply(1:catch_n , function(c) cor(x= jun_mn_q[,c], y= mar_sci[,10,c], method = "pearson", use="na.or.complete")) #10 because i am interested in spei-3, mar_sci created in script: sci_analysis

cor_jun_mar_spi= sapply(1:catch_n , function(c) cor(x= jun_mn_q[,c], y= mar_sci[,4,c], method = "pearson", use="na.or.complete")) #4 because i am interested in spi-3, mar_sci created in script: sci_analysis

data_plot =cbind.data.frame(cor_jun_mar_spei, sr=gauges$sr_new)



ggplot()+
  geom_boxplot(data=data_plot, aes(x=as.factor(sr), y=cor_jun_mar_spei))+
  ylim(c(0,.75))+
  xlab("Lowflow season")+
  scale_x_discrete(labels = c("summer", "winter"))+
  ylab("pearson correlation of mean q june ~ spei-3 (march)")

ggsave("./plots/3_choice/cor_jun_mar_spi.pdf")

pdf("./plots/3_choice/cor_ssi_spei_winter_vs_summer.pdf")
grid.arrange(winter, summer, ncol=2)
dev.off()



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

hist(gauges$sr_new) # normal
hist(mmky_mar_mn_q$sen_slope[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)][gauges$sr_new==0]) #not normal
hist(mmky_mar_mn_q$sen_slope[which(mmky_jun_mn_q$new_p<.05 & mmky_mar_mn_q$new_p<.05)][gauges$sr_new==2]) # not normal

y= mmky_jun_mn_q # mmky_jun_mn_q
x1= mmky_mar_mn_q # mmky_mar_mn_q 
x2= gauges$sr_new #or gauges$sr

lm_y = y$sen_slope[which(x1$new_p<fs_jun_mn_q & y$new_p< fs_mar_mn_q)]
lm_x1 = x1$sen_slope[which(x1$new_p<fs_jun_mn_q & y$new_p< fs_mar_mn_q)]
lm_x2 = x2[which(x1$new_p<fs_jun_mn_q & y$new_p<fs_mar_mn_q)]
#alp = alpine[which(x1$new_p<.05 & y$new_p<.05)]
data_plot = cbind.data.frame(lm_y,lm_x1, lm_x2)#, alp)
sig_points = which(x1$new_p<fs_jun_mn_q & y$new_p< fs_mar_mn_q)
#transformation to normal
lm_x1_w = lm_x1[which(lm_x2 == 2)] #winter lf
lm_x1_s = lm_x1[which(lm_x2 != 2)] #not winter low flows
hist(lm_x1_w)
hist(lm_x1_s)
lm_y_norm = abs(min(lm_y))+lm_y # adding minim
lm_x1_w_norm= (lm_x1_w+abs(min(lm_x1_s))) 
lm_x1_s_norm= (lm_x1_s+abs(min(lm_x1_s))) #adding the minima of summer (!) to both summer and winter
lm_x1_norm =  abs(min(lm_x1)-0.0000001)+lm_x1 # adding the minima +0.0000001 to enable log transform of the data tp normalize


#lm_x1_w_norm[which(lm_x1_w_norm == 0)] = 0.01
hist(log(lm_x1_s_norm)) #normal
hist(log(lm_x1_w_norm))#normal
hist(exp(lm_y_norm)) #normal

lm_y_norm %>% .^5 %>% hist
data_lm = cbind.data.frame(lm_y_norm,lm_x1_norm, lm_x2)
#head(data_df)
#problem one variable is normal the other is positivly skewed
fm = lm((lm_y_norm)^5 ~ log(lm_x1_norm)*lm_x2, data=data_lm)
fm2= lm((lm_y_norm)^5 ~ log(lm_x1_norm)*lm_x2, data=data_lm[-c(49,59,154, 174),]) #[-c(42,50,52,143,172,181),]) #without points with high leverage
summary(fm)
#what are the catchments with hich leverage
data_lm[c(49,59,154, 174),]
#can not be thrown out, are not "measuring" errors

plot(fm2)
hist(residuals(fm))


#problem residuals are not normal distributed, low variance for high fitted values and vice versa 
#now throught normalisation ok
# and high leverage through few points
cook_d = influence.measures(fm) %>% as.data.frame()#with threshhold =1
 range(cooks.distance(fm))
 
 table= cooks.distance(fm)
 which.max(table)
lm_x2[ which(table > .04)]
 table[c(41,42,52,192, 172)]

#without the following data points re run a linear modell 41,42,52,192
 #or without 42 50 52 143 172 181
error_jun_mn_q = error.bar("jun_mn_q")
 
ggplot(data= data_plot, aes(y=lm_y, x=lm_x1, col=as.factor(lm_x2)))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
 geom_point(data=  data_plot[data_plot$alp==0 & lm_x2 == 2,] , aes(x=lm_y, y=lm_x1), col="blue", show.legend = F)+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -2.5, label=paste("n = ", length(lm_y)))+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -1, label=paste(paste("p =", round(fs_mar_mn_q,3))))+
  annotate(geom="text", -Inf, -Inf,  hjust =-0.2, vjust = -4, label=paste("r²=",round(summary(fm)$adj.r.squared,2)))+
  ylab(paste("June mean q trend (slope) [m³/s/a]"))+
  xlab(paste("March mean q trend (slope) [m³/s/a]"))+
 geom_linerange(alpha= .4,aes( ymin=low_lim, ymax = upp_lim ), data = error_jun_mn_q[sig_points,])+
  scale_color_discrete("Seasonality", label=c("Summer", "Winter"))


#this is linear model with normal distribution not with selm and skewed normal distr

ggsave("./plots/trend_analysis/jun_march_v3.pdf")

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


#lm of ms7_date####
x1= mmky_ms7_date
y = mmky_su_sm_p

lm_y = y$sen_slope[which(x1$new_p<1 & y$new_p<1)]
lm_x1 = x1$sen_slope[which(x1$new_p<1 & y$new_p<1)]
#alp = alpine[which(x1$new_p<.05 & y$new_p<.05)]
data_plot = cbind.data.frame(lm_y,lm_x1)

#transformation to normal
hist(lm_y)
lm_y_norm= (lm_y+abs(min(lm_y))) 

nnn = (lm_x1 - mean(lm_x1))/sd(lm_x1)
hist((lm_y_norm))
hist(exp(nnn))

fm = lm(sqrt(lm_y_norm) ~ lm_x1)
summary(fm)

ggplot(data= data_plot, aes(y=lm_y, x=lm_x1))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE)+
  annotate(geom="text", Inf, Inf,  hjust = 1, vjust = 2, label=paste("n = ", length(y)))+
  annotate(geom="text", Inf, Inf,  hjust = 1, vjust = 4, label=paste("p = 0.05"))+
  annotate(geom="text", Inf, Inf,  hjust =1, vjust = 6, label=paste("r²=",round(summary(fm)$adj.r.squared,2)))+
  xlab(paste("mmky Summer q10 sen's slope"))+
  ylab(paste("mmky Wintzer q10 sen's slope"))




#stepwise regression ####
mmky_ms7_min$sen_slope %>% hist()
y = mmky_ms7_min$sen_slope
mmky_
step_fm=step(lm(mmky_ms30_min$sen_slope ~ mmky_wi_mn_t$sen_slope+mmky_su_mn_t$sen_slope + mmky_yearly_max_t$sen_slope + mmky_su_sm_p$sen_slope + mmky_wi_mn_t$sen_slope, mmky_su_p_pet$sen_slope, mmky_yr_days_below_0$sen_slope, mmky_wi_sm_p$sen_slope, mmky_yearly_mn_t$sen_slope ), direction = "forward" , k=log(catch_n))
summary(step_fm)
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


  
  
# correlation of ms7 with spi_n of that equivalant month####
ms7_spi_t =   ms7_min %>% 
    mutate(yr_mt = ymd(paste0(year(ms7_date_long), "-",month(ms7_date_long), "-15")))
  
  mat=matrix(nrow=catch_n, ncol=4)
  for (i in 1:catch_n){
  
spi_temp = filter(spi_lt_long, gauge==i)
ms7_temp = filter(ms7_spi_t, gauge==i)
ms7_spi=  spi_temp[pmatch(ms7_temp$yr_mt, spi_temp$yr_mt),3:6]
mat[i,] = cor(x=ms7_temp$ms7_min, y=ms7_spi, use="na.or.complete" , method = "s")

# ggplot()+
#   geom_point(aes(x= ms7_temp$yr_mt, y=ms7_temp$ms7_min ))+
#   geom_point(aes(y= ms7_spi$spi_06,
#                  x= ms7_temp$yr_mt), col=2)+
#   geom_hline(yintercept = mean(ms7_temp$ms7_min))+
#   xlab("catchment: 122")+
#   ylab("ms7_min [m³/s] & SPI-6 of the month of ms7_date")
# ggsave("./plots/5_choice/ms7_spi12_example.png")
  } 
  
  remove(ms7_spi_t,spi_temp ,ms7_temp, ms7_spi)
  
ms7_spi_cor = mat %>% 
  as.data.frame() %>% 
   set_colnames(c("spi_03", "spi_06", "spi_12", "spi_24") )%>% 
  mutate(gauge=1:catch_n) %>% 
  mutate(bfi = gauges$bfi,saar= gauges$saar, cor_drought_spi = gauges$cor_spi_dr, cor_drought_spi_n  = gauges$cor_spi_n_dr) %>% 
  as.tbl() %>% 
  gather(key=spi_n, value=spi, -bfi, -saar, -cor_drought_spi, -cor_drought_spi_n, -gauge)


ms7_best_spi_cor = ms7_spi_cor %>% 
  group_by(gauge) %>% 
  summarise(cor= max(spi), best_spi= spi_n[which.max(spi)]) %>% 
  mutate(bfi = gauges$bfi,saar= gauges$saar, cor_drought_spi = gauges$cor_spi_dr, cor_drought_spi_n  = gauges$cor_spi_n_dr) 
 
  
ggplot(data=ms7_spi_cor)+
  geom_point(aes(x=cor_drought_spi_n, y=spi, col=spi_n), position = position_dodge(width = .3))

ggplot(data= ms7_best_spi_cor)+
  geom_point(aes(x=saar, y=cor,  col=best_spi))+
  ylab("best spearman cor. ms7 ~ spi-n")

ggsave("./plots/5_choice/ms7_spi_n_cor.png")
  
# jun negative trend ####

y= mmky_jun_mn_q
x1= mmky_sp_sm_p
x2=gauges$sr_new
lm_y = y$sen_slope[which(x1$new_p<.05 & y$new_p<.05)]
lm_x1 = x1$sen_slope[which(x1$new_p<.05 & y$new_p<.05)]
lm_x2 = x2[which(x1$new_p<.05 & y$new_p<.05)]
lm_y_norm = abs(min(lm_y))+lm_y+0.0001
hist(sqrt(lm_y_norm))

fm = lm(lm_y~lm_x1*lm_x2)
summary(fm)

#winter sommer precipitation ####

lm(mmky_wi_p_pet$sen_slope ~ mmky_wi_mn_q$sen_slope) %>% summary()
lm(mmky_su_p_pet$sen_slope ~ mmky_su_sm_p$sen_slope) %>% summary()

#interaction catchment characteristics####
data_plot = cbind.data.frame()

glm(mmky_ms30_min$sen_slope[mmky_ms30_min$new_p < fs_ms30] ~ gauges$bfi[mmky_ms30_min$new_p < fs_ms30] * gauges$sr_new[mmky_ms30_min$new_p < fs_ms30]) %>% summary()

ggplot(data= data_plot, aes(y=lm_x1, x=lm_y, col=as.factor(lm_x2)))+
  geom_point()+
  geom_smooth(method="lm", se = TRUE, show.legend = F)+
 geom_point(data=  data_plot[data_plot$alp==0 & lm_x2 == 2,] , aes(x=lm_y, y=lm_x1), col="blue", show.legend = F)+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -2.5, label=paste("n = ", length(lm_y)))+
  annotate(geom="text", -Inf, -Inf,  hjust = -0.2, vjust = -1, label=paste("p = 0.05"))+
  annotate(geom="text", -Inf, -Inf,  hjust =-0.2, vjust = -4, label=paste("r²=",round(summary(fm)$adj.r.squared,2)))+
  ylab(paste("mmky June mean q sen's slope"))+
  xlab(paste("mmky March mean q sen's slope"))+
  scale_color_discrete("Seasonality", label=c("summer", "winter"))

#correlation seasonal temp and discharge
data_plot =cbind.data.frame(summer = cor(x=su_mn_q, y=su_mn_t) %>% diag, winter = cor(x=wi_mn_q, y=wi_mn_t) %>% diag, sr=gauges$sr_new)
ggplot(data_plot)+
  geom_point(aes(x=summer, y=winter, col=as.factor(sr)))

data_plot =cbind.data.frame(summer = cor(x=su_q10, y=su_mn_t) %>% diag, winter = cor(x=wi_q10, y=wi_mn_t) %>% diag, sr=gauges$sr_new)
ggplot(data_plot)+
  geom_point(aes(x=summer, y=winter, col=as.factor(sr)))+
  xlab("pearson cor. summer q10 ~ mean temp ")+
    ylab("pearson cor. winter q10 ~ mean temp ")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
 ggsave("./plots/drought_attribution/pearson_cor_q10.png")

cor(x=mmky_su_q10$sen_slope, y=mmky_wi_q10$sen_slope)
cor(x=mmky_wi_sm_p$sen_slope, y=mmky_wi_med_q$sen_slope)
cor(x=wi_sm_p, y=wi_med_q)%>% diag %>% hist()
cor(x=su_sm_p[gauges$sr_new == 0], y=su_med_q[gauges$sr_new == 0])%>% diag %>% hist()
cor(x=su_sm_p[gauges$sr_new == 2], y=su_med_q[gauges$sr_new == 2])%>% diag %>% hist()
cor(x=yearly_sm_p, y=yearly_mn_q)%>% diag %>% hist()

data_plot =cbind.data.frame(summer = cor(x=su_sm_p, y=su_med_q)%>% diag, winter = cor(x=wi_sm_p, y=wi_med_q) %>% diag, sr=gauges$sr_new)
ggplot(data_plot)+
  geom_point(aes(x=summer, y=winter, col=as.factor(sr)))+
  xlab("pearson cor. summer median q~ precipitation sum ")+
    ylab("pearson cor. winter median q ~ precipitation sum ")+
  scale_color_discrete("Seasonality", labels=c("summer","winter"))
 ggsave("./plots/drought_attribution/pearson_cor_med_q_p.pdf")
 
 #drought attribution correlation monthly pet and t and p ~ monthly q trends####
 
# res1=monthly.cor(p_y="fs_mt_med_q", p_x="fs_mn_t", cor_x= "_med_t", cor_y= "_med_q", no_sig=T)
mn_t= monthly.cor(p_y="fs_mt_med_q", p_x="fs_mn_t", cor_x= "_med_t", cor_y= "_med_q", no_sig=F)
p_pet= monthly.cor(p_y="fs_mt_med_q", p_x="fs_mt_p_pet", cor_x= "_p_pet", cor_y= "_med_q", no_sig=F)
prec = monthly.cor(p_y="fs_mt_med_q", p_x="fs_sm_p", cor_x= "_sm_p", cor_y= "_med_q", no_sig=F)
data_plot = cbind.data.frame(mn_t[,1], p_pet[,1], prec[,1]) %>% 
  set_colnames(c("med_t","p_pet","prec")) %>% 
  mutate(month=1:12) %>% 
  gather(value=cor, key=meteo, -month) %>% 
  as.tbl

ggplot(data_plot)+
  geom_line(aes(x=month, y=cor,col=as.factor(meteo)))+
  xlab("")+
  ylab("pearson correlation with median monthly discharge trends")+
  scale_x_continuous(breaks=c(1:12), labels=month.abb)+
  scale_color_discrete("Meteorology",labels=c)


#meteo aggregated (similar to spi-n)


precip_agg = agg.meteo(dat=mt_sm_p_wide, fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q", subset=NULL, cor=F)
precip_agg_cor = agg.meteo(dat=mt_sm_p_wide, fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q", subset=NULL, cor=T)
p_pet_agg = agg.meteo(dat=year_p_pet, fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q", subset=NULL, cor=F)
p_pet_agg_cor = agg.meteo(dat=year_p_pet, fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q", subset=NULL, cor=T)
# p_pet_temp = agg.meteo(dat=mt_mn_temp %>% spread(.,key=gauge, value=temp_m) %>% dplyr::select(-yr_mt), fs=0.02967359, agg_t = agg_month, cor_y = "_mn_q", subset=which(gauges$sr_new ==0))



save(file="./output/agg_meteo.Rdata", list=c("precip_agg","p_pet_agg","precip_agg_cor","p_pet_agg_cor"))

load("./output/agg_meteo.Rdata", verbose=T)

#analsis of correlation of meteorologic trends with flow trends on monthly basis




plot(x=1:12, y=precip_agg[[2]][,2], type="l")

data_plot_spi = precip_agg[[2]] %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>% 
  gather(., key= agg_month, value=cor,-month)

data_plot_spei = p_pet_agg[[2]] %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  as.data.frame() %>% 
  mutate(month = 1:12) %>% 
  gather(., key= agg_month, value=cor,-month)

ggplot(data_plot_spei)+
  geom_smooth(aes(x=month, y=cor, col=as.factor(agg_month)), se=F)

ggplot(data_plot_spei)+
  geom_line(aes(x=month, y=cor, col=as.factor(agg_month)), se=F)

#correlation of actual flow and metreologic variables on monthly basis

mn_cor_precip = sapply(1:6, function(x) apply(precip_agg_cor[[x]],2,mean))
#sd_cor_precip = lapply(1:6, function(x) apply(precip_agg_cor[[x]],2,quantile, probs=c(.25,.75)))  
mn_cor_p_pet = sapply(1:6, function(x) apply(p_pet_agg_cor[[x]],2,mean))

data_plot = mn_cor_precip %>% 
  as.data.frame() %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  mutate(month= 1:12) %>% 
  gather(.,key=agg_month, value=cor, -month ) %>% 
  as.tbl()

ggplot(data_plot)+
  geom_line(aes(x=month, y=cor, col=(agg_month)))

spi = ggplot(data_plot %>% filter(.,agg_month != "24"))+
  geom_smooth(aes(x=month, y=cor, col=(agg_month)), span=.2, se=F)+
  scale_color_discrete("Aggregation month")+
  scale_x_continuous("", breaks=seq(2,12,2), labels = month.abb[seq(2,12,2)])+
  labs(y="pearson correlation")+
  nice

data_plot2 = mn_cor_p_pet %>% 
  as.data.frame() %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  mutate(month= 1:12) %>% 
  gather(.,key=agg_month, value=cor, -month ) %>% 
  as.tbl()

ggplot(data_plot2)+
  geom_line(aes(x=month, y=cor, col=(agg_month)))

spei = ggplot(data_plot2 %>% filter(.,agg_month != "24"))+
  geom_smooth(aes(x=month, y=cor, col=(agg_month)), span=.2, se=F)+
  scale_color_discrete("Aggregation month")+
  scale_x_continuous("", breaks=seq(2,12,2), labels = month.abb[seq(2,12,2)])+
  labs(y="")+
  nice

p= grid.arrange(spi, spei,ncol=2)
ggsave(plot = p, "./plots/drought_attribution/monthly_sci.pdf")

#one united plot of precipi and p-pet

data_plot3 = mn_cor_p_pet %>% 
  as.data.frame() %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  dplyr::select("02","03","06") %>% 
  set_colnames(c("spi01","spi03","spi06")) %>% 
  mutate(month= 1:12) %>% 
  cbind(mn_cor_p_pet[,2:4] %>% set_colnames(c("spei01","spei03","spei06")),.) %>% 
  gather(.,key=agg_month, value=cor, -month ) %>% 
  as.tbl()

ggplot(data_plot3)+
  geom_line(aes(x=month, y=cor, col=(agg_month)))


# drought ssi spi/spie correlation heatmaps -------------------------------

drought_sci_0 = dr_corr(threshhold = 0)

cor_spi = matrix(nrow=catch_n, ncol=length(drought_sci_0))
cor_spei = matrix(nrow=catch_n, ncol=length(drought_sci_0))

for (a in 1:length(drought_sci_0)){
for (g in 1:catch_n){
temp= drought_sci_0[[a]] %>% 
  filter(gauge== g)
cor_spi[g, a] = cor(y= temp$ssi , x= temp$spi, use="c", method = "spearman") 
cor_spei[g, a] = cor(y= temp$ssi , x= temp$spei, use="c", method = "spearman")
}
}


plotly::plot_ly(x= 1:6, y=1:337, z=cor_spei[bfi_id,], type="heatmap")
plotly::plot_ly(x= 1:6, y=1:337, z=cor_spei[bfi_id,], type="heatmap")


data_plot_spi = cor_spi %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  as.data.frame() %>% 
  mutate(bfi_class = gauges$bfi_class, sr= gauges$sr_new, geo= gauges$hydrogeo_simple ) %>% 
  gather(., key= agg_month, value=cor,-bfi_class, -sr,-geo)

data_plot_spei = cor_spei %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  as.data.frame() %>% 
  mutate(bfi_class = gauges$bfi_class, sr= gauges$sr_new, geo= gauges$hydrogeo_simple ) %>% 
  gather(., key= agg_month, value=cor,-bfi_class, -sr,-geo)


ggplot(data_plot_spei)+
  geom_boxplot(aes(x= agg_month, y= cor, col=as.factor(sr)))+
  labs(x="Aggregation Period",
       y= "spearman correlation spei-n ~ ssi-1")+
  scale_color_discrete("BFI class")
ggsave("./plots/drought_attribution/cor_spi_ssi.pdf")

ggplot(data_plot_spi)+
  geom_boxplot(aes(x= agg_month, y= cor, col=as.factor(bfi_class)))+
  labs(x="Aggregation Period",
       y= "spearman correlation spi-n ~ ssi-1")+
  scale_color_discrete("BFI class")+
  theme_bw()
ggsave("./plots/drought_attribution/cor_spi_ssi.pdf")
lm(mn_deficit ~ bfi, data=gauges) %>% summary

# drought correlation per year####
#during drought years, what was the major influence SPI or SPEI and which aggregation month

cor_spi_yr = matrix(nrow=40, ncol=length(drought_sci_0))
cor_spei_yr = matrix(nrow=40, ncol=length(drought_sci_0))

for(a in 1:6){
  for(y in 1970:2009){
temp= drought_sci_0[[a]] %>% 
  filter(y == year(yr_mt))
    
cor_spi_yr[y-1969, a] = cor(y= temp$ssi , x= temp$spi, use="pairwise.complete.obs", method = "spearman") 
cor_spei_yr[y-1969, a] = cor(y= temp$ssi , x= temp$spei, use="pairwise.complete.obs", method = "spearman")
  }
}

data_plot=cor_spi_yr %>% 
  as.data.frame() %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  mutate(year = 1970:2009) %>% 
  as.data.frame() %>% 
    gather(., key= agg_month, value=cor,-year)

data_plot_spei=cor_spei_yr %>% 
  as.data.frame() %>% 
  set_colnames(c("01","02","03","06","12","24")) %>% 
  mutate(year = 1970:2009) %>% 
  as.data.frame() %>% 
    gather(., key= agg_month, value=cor,-year)


spi = ggplot(data= data_plot)+
  geom_line(aes(x=year, y=cor, col=agg_month), lwd=1.1, show.legend = T)
#,lwd=1.2, se=F, method = "loess", span=.2)

spei= ggplot(data= data_plot_spei)+
  geom_line(aes(x=year, y=cor, col=agg_month), lwd=1.1, show.legend = F)

grid.arrange(spi,spei,ncol=2)

