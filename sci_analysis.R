
# SCI Analysis ------------------------------------------------------------

source("./R/masta_v1/functions.R")# has to run before if not objects will be missin!
source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!
source("./R/masta_v1/sci_calculation.R")# has to run before if not objects will be missin!
# autocorrelation ---------------------------------------------------------
acf(spei_1)

# cross correlation -------------------------------------------------------

ccf(x= spi_12[,1], y=ssi_1[,1], na.action=na.pass)

ccf_spi <- sci_ccf(sci= c(1,2,3,6,12,24),sci_namex = "spi_", sci_namey="ssi_1")
ccf_spei <- sci_ccf(sci= c(1,2,3,6,12,24), sci_namex="spei_", sci_namey="ssi_1")

#one can see that advancing the spei value leads to considerly less correlation after the spei_n lag (positive) the correlation drops dramatically. retropespective the correlation decreases slower. Meaning that spei leads ssi. 
# 

png("")
plot(x=1:catch_n, y=ccf_spi_v2[[1]][,2])
points(x=1:catch_n, y=ccf_spi[[1]][,2], col=2)


# pdf("./plots/boxplot_ccf_spei_acf_new.pdf")
# boxplot(ccf_spei[[1]], xlab="SPEI-n", ylab="acf")
# dev.off()
# 
# pdf("./plots/boxplot_ccf_spei_lag_new.pdf")
# boxplot(ccf_spei[[2]], xlab="SPEI-n", ylab="lag")
# dev.off()
# 
# pdf("./plots/boxplot_ccf_spi_acf_new.pdf")
# boxplot(ccf_spi[[1]], xlab="SPI-n", ylab="acf")
# dev.off()
# 
# pdf("./plots/boxplot_ccf_spi_lag_new.pdf")
# boxplot(ccf_spi[[2]], xlab="SPI-n", ylab="lag")
# dev.off()

#gauge 72 has high lag why? its real catchment number is 94
unique(mt_mn_temp$gauge)[72]

ccf(x = spi_v2_7$V72, y = ssi_sorted$V72, na.action = na.pass)
plot(spi_v2_7$V72, type="l")
lines(ssi_sorted$V72, col=2)

# mt_mn_temp %>% 
#   filter(gauge==94) %>% 
# ggplot()+
#   geom_smooth(aes(y=temp_m, x= yr_mt), span=.05)+
#   geom_smooth(data= precip_monthly %>% filter(gauge==94), aes(y=month_sum, x= yr_mt), span=.05)+
#    geom_line(data= mt_mn_q %>% filter(gauge==94), aes(y=q_mean, x= yr_mt), span=.05)+
#   scale_y_log10()


# pdf("./plots/spei_trend.pdf")
# boxplot(ken_spei[[1]])
# dev.off()
# 
# pdf("./plots/spi_trend.pdf")
# boxplot(ken_spi[[1]])
# dev.off()


# pdf("./plots/spei_ssi_trend.pdf")
# plot(y= median(as.numeric(ken_spei[[1]][1,])), x=ken_ssi[1,1], type="p", ylim=c(-0.4,0.4), xlim=c(-0.6,0.4))
# for (i in 1:nrow(ken_spei[[1]])) lines(y=median(as.numeric(ken_spei[[1]][i,])),x=ken_ssi[i,1], type="p")
# dev.off()

# pdf("./plots/spi_ssi_trend.pdf")
# plot(y= median(as.numeric(ken_spi[[1]][1,])), x=ken_ssi[1,1], type="p", ylim=c(-0.4,0.4), xlim=c(-0.6,0.4))
# for (i in 1:nrow(ken_spi[[1]])) lines(y=median(as.numeric(ken_spei[[1]][i,])),x=ken_ssi[i,1], type="p")
# dev.off()

plot(y=ken[[1]][1,],x=1:agg_month, type="l", ylim=c(-.5, .5))
for (i in 2:50) lines(y=ken[[1]][i,],x=1:agg_month, type="l")


# kendall rank correlation -----------------------------------------------------

mk_spi_tau <- list()
mk_spi_S <- list()
mk_spi_D <- list()
mk_spi_p <- list()
for (i in 1:ncol(spi_2)){
 mk_spi_tau[[i]] <-  Kendall(spi_2[,i], ssi_1[,i])$tau[1] 
 mk_spi_S[[i]] <- Kendall(spi_2[,i], ssi_1[,i])$S[1] 
mk_spi_D[[i]] <- Kendall(spi_2[,i], ssi_1[,i])$D[1] 
mk_spi_p[[i]] <- Kendall(spi_2[,i], ssi_1[,i])$sl[1] 
}

Kendall(spi_1[,44], ssi_1[,44])
#tau ist S/D
#S anzahl an positiven - negativen trends
#D value theoretisch mögliche maximale anzahl an trends
plot(x =spi_2[,1] , y=ssi_1[,1])
#offensichtliche aussage: desto höher der spi, desto höher der spei 



# drought attribution: SPI or SPEI? with linear regression ####


spi_ssi = spi_spei_reg(sci = "spi_") #1 =intercept 2= slope 3 = rsq
spei_ssi = spi_spei_reg(sci = "spei_") 
best_spi = c()
value_spi = c()
best_spei = c()
value_spei = c()

plot(spi_ssi[[3]][,3])
points(spi_ssi_v2[[3]][,3], col=2)

for(r in 1:catch_n){
 best_spi[r] = spi_ssi[[3]][r,] %>% which.max()
 value_spi[r] = spi_ssi[[3]][r,] %>% max()}
gauges$reg_spi_n = best_spi
gauges$reg_spi_n = value_spi
for(r in 1:catch_n){
 best_spei[r] = spei_ssi[[3]][r,] %>% which.max()
 value_spei[r] = spei_ssi[[3]][r,] %>% max()}

gauges$reg_spei_n = best_spei
gauges$reg_spei_n =value_spei

# pdf("./plots/spi_spei_reg.pdf")
# plot(best_spi - best_spei)
# dev.off()
# 
# pdf("./plots/spi_spei_reg_rsq.pdf")
# plot(value_spi, ylab="best r²")
# points(value_spei, col=2)
# legend("bottomleft", col=c(1,2), pch=c(1,1), c("spi", "spei"), bty="n")
# dev.off()
# 
# pdf("./plots/spi_spei_opt_agg_n.pdf")
# plot(best_spi, ylab="SPI-/SPEI-n with lowest r²")
# points(x=which(best_spei != best_spi), y=best_spei[best_spei != best_spi], col=2)
# legend("topleft", c("spi", "spei(only if diff. to spi)"), col=c(1,2), pch=c(1,1), bty="n")
# dev.off()



# drought attribution: SPI or SPEI? with correlation ####

cor_spi_ssi_v2 = cor_sci_ssi(sci_n= c(1,2,3,6,12,24), cor_met="p", sci="spi_v2_", ssi="ssi_1")
cor_spei_ssi = cor_sci_ssi(sci_n= c(2), cor_met="p", sci="spei_", ssi="ssi_1")

plot(cor_spi_ssi[,3] ~ spi_ssi[[2]][,3])

png("cor_spi_v2_spi_np.png")
plot(cor_spi_ssi[,3],ylab="correlation of SPI-3" )
points(cor_spi_ssi_v2[,3], col=2)
legend("bottomleft", col=c(1,2), pch=c(1,1), c("non-paramatric", "parametric"), bty="n")
dev.off()

best_spi = c()
  value_spi = c()
best_spei = c()
  value_spei = c()
for(r in 1:catch_n){
 best_spi[r] = cor_spi_ssi[r,] %>% which.max()
 value_spi[r] = cor_spi_ssi[r,] %>% max()}

for(r in 1:catch_n){
 best_spei[r] = cor_spei_ssi[r,] %>% which.max()
 value_spei[r] = cor_spei_ssi[r,] %>% max()}

gauges$cor_spei_n = best_spei
gauges$cor_spi_n = best_spi
gauges$cor_spi = value_spi
gauges$cor_spei = value_spei

ggplot()+
  geom_point(aes(y= mmkh_ms7_min$Tau, x=value_spi, col= gauges_df$bfi))+
  ylab("mmkh tau of mnq7 summer")+
  xlab("correlation spi-n")+
  scale_color_continuous("BFI")

ggsave("mnq7_summer_date_min.png")

ggplot()+
  geom_point(aes(x= mmkh_ms7_min$Tau , mmkh_ms7_date$Tau, col=gauges_df$bfi))+
  geom_abline(slope=1, intercept = 0)+
  ylab("mmkh mnq7 summer date")+
  xlab("mmkh mnq7 summer")+
  scale_color_continuous("BFI")

ggplot()+
  geom_point(aes(y= mmkh_ms7_min$Tau , x= cor_spei_ssi$`2`, col=gauges_df$bfi))+
  ylab("mmkh mnq7 summer tau")+
  xlab("corr. SPEI-2 ~ SSI-1")+
  scale_color_continuous("BFI")

# pdf("./plots/spi_spei_cor.pdf")
# plot(value_spi, ylab="pearson correlation")
# points(value_spei, col=2)
# legend("bottomleft", col=c(1,2), pch=c(1,1), c("spi", "spei"), bty="n")
# dev.off()
# 
# pdf("./plots/spi_spei_cor_optim_n.pdf")
# plot(best_spi, ylab="Otim. SPI-/SPEI-n")
# points(x=which(best_spei != best_spi), y=best_spei[best_spei != best_spi] , col=2)
# legend("topleft", col=c(1,2), pch=c(1,1), c("spi", "spei (only if diff. to spi)"), bty="n")
# dev.off()

# pdf("./plots/spi-ssi_regression.pdf")
# boxplot(cor_spi_ssi)
# dev.off()


#why some values are really low?

worst_spei =value_spei %>% order(., decreasing = FALSE) %>% head(10)
worst_spi =value_spi %>% order(., decreasing = FALSE) %>% head(10)
value_spei[worst_spei]
value_spi[worst_spi]

par(mfrow=c(2,1))

plot(mt_mn_q_wide$`72`, t="l", ylim=c(-2,2), ylab="72 SPI-6/q [m³/s]")
par(axis(4))
lines(spi_6$`72`, col=2)



ggplot()+
  geom_line(data= mt_mn_q %>% filter(gauge == 72), aes(x=yr_mt, y=q_mean))

  ggplot()+
geom_line(data=spi_6, aes( x= as.Date(yr_mt, origin="1970-01-01"), y=`72`))

dev.off()

grangertest(x=spi_v2_2$V12,y=ssi_sorted[,12], order=1)


heatmap(spi_ssi_c, Colv = NA, Rowv = NA, scale="column")
my_palette <- colorRampPalette(c("red", "blue"))(n = 199)

# gplots::heatmap.2(spi_ssi_c[gauges$ezggr_class == "<50",],
#     # same data set for cell labels
#   main = "Correlation SPI-n + SSI-1", # heat map title
#   notecol="black",      # change font color of cell labels to black
#   density.info="none",  # turns off density plot inside color legend
#   trace="none",         # turns off trace lines inside the heat map
#  # margins =c(12,9),     # widens margins around plot
#   col=my_palette,       # use on color palette defined earlier
#  # breaks=col_breaks,    # enable color transition at specified limits
#   dendrogram="none",     # only draw a row dendrogram
#   Colv="F",               # turn off column clustering
#  Rowv = "F"
#  ) 



# boxplot(spi_ssi_c, horizontal = F)
# opt_spei_n <- c()
# for (i in 1:length(spei_ssi_c[,1])){
# opt_spei_n[i] <- which.max(spei_ssi_c[i,])
# }
# opt_spi_n <- c()
# for (i in 1:length(spi_ssi_c[,1])){
# opt_spi_n[i] <- which.max(spi_ssi_c[i,])
# }
# gauges$optim_spi_p  <- opt_spi_n
# gauges$optim_spei_p <- opt_spei_n
# spplot(gauges, "optim_spi_p")

# pdf("./plots/opt_spei_n.pdf")
# plot(x=1:length(spei_ssi_c[,1]), y=opt_spei_n, xlab="Catchments", ylab="SPI-n with highest cor")
# points(x=1:length(spei_ssi_c[,1]), y=opt_spi_n, col=2)
# dev.off()

# pdf("./plots/opt_spi-spei_n._spearman.pdf")
# plot(x=1:length(spei_ssi_c[,1]), y=opt_spi_n-opt_spei_n, xlab="Catchments", ylab="optim. SPI-n - optim. SPEI-n")
# dev.off()
gauges$



gam( ssi_sorted$V1~s(spi_v2_1$V1)+s(spei_v2_1$V1)) %>% summary()
lm( ssi_sorted$V1~spi_v2_1$V1+spei_v2_1$V1) %>% summary()
nls(ssi~spi, data=test)
lo <- loess(ssi~spi,data=test, span=.6,method = "l" )

lm(ssi_sorted$V1 ~ spei_v2_1$V1) %>% summary()
predicted.intervals <- predict(rm,data.frame(x=spei_v2_1$V1),interval='confidence',level=0.99)
plot(ssi_sorted$V1~spei_v2_1$V1, t="p")
lines(spei_v2_1$V1[order(spei_v2_1$V1)],predicted.intervals[,1][order(predicted.intervals[,1])],col='green',lwd=3)
lines(spei_v2_1$V1[order(spei_v2_1$V1)],predicted.intervals[,2][order(predicted.intervals[,1])],col=1,lwd=3)
lines(spei_v2_1$V1[order(spei_v2_1$V1)],predicted.intervals[,3][order(predicted.intervals[,1])],col=1,lwd=3)



# monthly correlation values for SPI/SPEI####
  

summer_cor_spi = cor_sci_ssi_sea(sci_n= c(1,2,3,6,12,24), cor_met="p", sci="spi_", ssi="ssi_1", begin=5, end=11) 
  
summer_cor_spei = cor_sci_ssi_sea(sci_n= c(1,2,3,6,12,24), cor_met="p", sci="spei_", ssi="ssi_1", begin=5, end=11) 


best_spi_summer=c()
best_spei_summer =c()
value_spei_summer = c()
value_spi_summer=c()

for(r in 1:catch_n){
 best_spei_summer[r] = summer_cor_spei[r,] %>% which.max()
 value_spei_summer[r] = summer_cor_spei[r,] %>% max()}
for(r in 1:catch_n){
 best_spi_summer[r] = summer_cor_spi[r,] %>% which.max()
 value_spi_summer[r] = summer_cor_spi[r,] %>% max()}

gauges$best_spei_summer = best_spei_summer
gauges$cor_spei_summer = value_spei_summer
gauges$best_spi_summer = best_spi_summer
gauges$cor_spi_summer = value_spi_summer


# stepwise regression ####
g=228
res = step(lm(ssi_1[,g]~spei_1[,g]+ spi_1[,g] + spei_2[,g]+ spi_2[,g]+ spei_3[,g]+spi_3[,g] + spei_6[,g] + spi_6[,g] + spi_12[,g] + spei_12[,g] + spi_24[,g] + spei_24[,g]),direction="both")


summary(res)
  extractAIC()
#longterm (lt) memory effect of catchments####
lt_cor_spi = cor_sci_ssi(sci_n = c(12,24), sci="spi_")
lt_cor_spei = cor_sci_ssi(sci_n = c(12,24), sci="spei_")

ggplot()+
  geom_point(aes(y=lt_cor_spi$`24`, x=gauges_df$bfi, col=as.factor(gauges_df$spi_n )))+
  ylab("cor. SPI-24 ~ SSI-1")+
  xlab("BFI")+
  scale_color_discrete("optim. \n SPI-n")

ggplot()+
  geom_point(aes(y=lt_cor_spi$`12`, x=gauges_df$bfi, col=gauges_df$Enzgsg_ ))+
  ylab("cor. SPI-12 ~ SSI-1")+
  xlab("BFI")+
  scale_color_continuous("Catchment \n Size [km²?]")
ggsave("memoryeffect_24.png")

    
#plots####
  gauges_df = gauges %>% as.data.frame()
  
  ggplot() +
      geom_point(data= gauges_df, aes(x=bfi,y=mnq30_month,  col=as.factor(sr)))+
    ylab("average month of lowest flow")+
    scale_color_discrete("Seasonality \n ratio", label=c("summer", "unclear", "winter"))+
    scale_y_continuous(breaks = seq(1,12,1), labels=month.abb)
  
  ggsave("bfi_lf_month.png")
    

  