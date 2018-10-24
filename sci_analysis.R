
# SCI Analysis ------------------------------------------------------------

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")
source("./R/masta_v1/sci_calculation.R")
# autocorrelation ---------------------------------------------------------
acf(spei_1)

# cross correlation -------------------------------------------------------




ccf_spei <- sci_ccf()
ccf_spi <- sci_ccf(sci_namex = "spi_v2")

#one can see that advancing the spei value leads to considerly less correlation after the spei_n lag (positive) the correlation drops dramatically. retropespective the correlation decreases slower. Meaning that spei leads ssi. 


# pdf("./plots/boxplot_ccf_spei_acf.pdf")
# boxplot(ccf_spei[[1]], xlab="SPEI-n", ylab="acf")
# dev.off()
# 
# pdf("./plots/boxplot_ccf_spei_lag.pdf")
# boxplot(ccf_spei[[2]], xlab="SPEI-n", ylab="lag")
# dev.off()
# 
# pdf("./plots/boxplot_ccf_spi_acf.pdf")
# boxplot(ccf_spi[[1]], xlab="SPI-n", ylab="acf")
# dev.off()
# 
# pdf("./plots/boxplot_ccf_spi_lag.pdf")
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


# SPEI vs. SPI comparison -------------------------------------------------

lm_spei_ssi <- spi_spei_reg(pred = "spei_v2") #[1]intercept, [2]slope [3] r²
lm_spi_ssi <- spi_spei_reg(pred="spi_v2")

sci_4_i <- sci_reg(sci_n = 4, interaction = T)


stat <- c()
for (i in 1:338){
  stat[i] <- sci_4_i[[i]]$adj.r.squared
}

pdf("./plots/sci_4_i_r2.pdf") #i interaction, ni no interaction
plot(stat, t="p")
dev.off()

#plots
# pdf("./plots/spi-ssi_regression.pdf")
# par(mfrow=c(1,3))
# boxplot(lm_spi_ssi[[1]], ylab="intercept", xlab="spi-n" ) #intercept
# boxplot(lm_spi_ssi[[2]], ylab="slope", xlab="spi-n") #slope
# boxplot(lm_spi_ssi[[3]], ylab="r²", xlab="spi-n") #r²
# dev.off()

# kendall rank correlation -----------------------------------------------------

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


# decompose time series into trend and seasonal part ----------------------
ssi_ts <- ts(ssi_sorted, start=c(1970,1), end=c(2009, 12), deltat = 1/12 )

ssi_dec <- decompose(ssi_ts[,1])
plot(ssi_dec)

#drought attribution: SPI or SPEI? with linear regression ####
lm_sci <- function(sci="spi_v1"){
  data <- get(sci)
  res <- matrix(nrow=catch_n, ncol=4)
for(g in 1:catch_n){
  fm <- lm(ssi[,g] ~ data[,g]) %>% summary()
  res[g,1] <- coef(fm) 
}
  return(res)
}
reg_spi_ssi <- lm_sci(sci="spi_v1")

spi_ssi_reg <- sci_reg(pred= "spi_v1", resp="ssi", interaction = FALSE)
spei_ssi_reg <- sci_reg(pred= "spei_v1", resp="ssi", interaction = FALSE)



plot_reg(spi_source = "spi_ssi_reg", spei_source="spei_ssi_reg", agg_n = 1)


#drought attribution: SPI or SPEI? with correlation ####





spi_ssi_np <- cor_sci_ssi_old(sci="spi", cor_met = "p", ssi = "ssi") #pearson
spei_ssi_np <- cor_sci_ssi_old(sci="spei_v2", cor_met = "p") #pearson


spi_ssi_c <- cor_sci_ssi_old(sci="spi_v2", cor_met = "p") #pearson
spei_ssi_c <- cor_sci_ssi_old(sci="spei_v2", cor_met = "p") #pearson

grangertest(x=spi_v2_2$V12,y=ssi_sorted[,12], order=1 )


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
opt_spei_n <- c()
for (i in 1:length(spei_ssi_c[,1])){
opt_spei_n[i] <- which.max(spei_ssi_c[i,])
}
opt_spi_n <- c()
for (i in 1:length(spi_ssi_c[,1])){
opt_spi_n[i] <- which.max(spi_ssi_c[i,])
}
gauges$optim_spi_p  <- opt_spi_n
gauges$optim_spei_p <- opt_spei_n
spplot(gauges, "optim_spi_p")

# pdf("./plots/opt_spei_n.pdf")
# plot(x=1:length(spei_ssi_c[,1]), y=opt_spei_n, xlab="Catchments", ylab="SPI-n with highest cor")
# points(x=1:length(spei_ssi_c[,1]), y=opt_spi_n, col=2)
# dev.off()

# pdf("./plots/opt_spi-spei_n._spearman.pdf")
# plot(x=1:length(spei_ssi_c[,1]), y=opt_spi_n-opt_spei_n, xlab="Catchments", ylab="optim. SPI-n - optim. SPEI-n")
# dev.off()

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


