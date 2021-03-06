
# SCI Analysis ------------------------------------------------------------

# source("./R/masta_v1/functions.R")# has to run before if not objects will be missin!
# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!
# source("./R/masta_v1/sci_calculation.R")# has to run before if not objects will be missin!

# cross correlation -------------------------------------------------------
sapply(1:catch_n, function(x) ccf(x= spi_v2_1[,x], y=ssi_1[,x], na.action=na.pass, plot=F)$acf)

for(i in agg_month){
  int = which(ssi_1[,x] < 0)
  if (int[1] == 1) {
    int= int[2:length(int)]
  }
  res= sapply(1:catch_n, function(x) cor(x= get(paste0("spei_v2_",i))[(int-2),x], y=ssi_1[int,x],method = "s" ,use = "na.or.complete"))
  assign(paste0("spei_",i,"_predict2"),res)
  
}
pdf(file="./plots/sci_analysis/spi_during_dr.pdf")
boxplot(gauges$cor_spi_dr~ gauges$cor_spi_n_dr, horizontal  = T, names= agg_month, ylab="SPI-n", xlab="spearman correlation") 
dev.off()

#cor during drought over mutlitple aggreagteion periods
mat = matrix(nrow=2*length(agg_month), ncol=2)
n=1

 for (i in agg_month){
   mat[n,1] = get(paste0("spi_",i)) %>% mean
   mat[n,2] = get(paste0("spi_",i)) %>% sd
   n=n+1
 }

n=7

 for (i in agg_month){
   mat[n,1] = get(paste0("spei_",i)) %>% mean
   mat[n,2] = get(paste0("spei_",i)) %>% sd
   n=n+1
 }
mat %<>% as.data.frame()
mat$sci = c(rep("spi",6),rep("spei",6))
mat$agg  = rep(agg_month,2)
colnames(mat) = c("mean","sd","sci","agg")

mat_final = mat %>% 
  mutate(sd_pos = mean+sd, sd_neg = mean-sd)

ggplot(mat_final)+
  #geom_point(aes(x=factor(agg), y= cor, col=factor(sci)))+
  xlab("aggregation period")+
  scale_color_discrete("SCI", labels=c("spi", "spei"))+
  #scale_x_continuous(breaks=c(1,2,3,6,12,24), labels=c("1","2","3","6","12","24"))+
  theme_bw()+
  ylab("spearman correlation of SCI ~ SSI-1")+
  geom_pointrange(aes(x=factor(agg), ymin = sd_neg, ymax=sd_pos,y=mean, col=factor(sci)), position = position_dodge(width=.3))

ggsave("./plots/sci_analysis/drought_propergation2.pdf")


#not the predictablitly

mat = matrix(nrow=2*length(agg_month), ncol=2)
n=1

 for (i in agg_month){
   mat[n,1] = get(paste0("spi_",i,"_predict1")) %>% mean
   mat[n,2] = get(paste0("spi_",i,"_predict1")) %>% sd
   n=n+1
 }

n=7

 for (i in agg_month){
   mat[n,1] = get(paste0("spei_",i,"_predict1")) %>% mean
   mat[n,2] = get(paste0("spei_",i,"_predict1")) %>% sd
   n=n+1
 }
mat %<>% as.data.frame()
mat$sci = c(rep("spi",6),rep("spei",6))
mat$agg  = rep(agg_month,2)
colnames(mat) = c("mean","sd","sci","agg")

mat_final = mat %>% 
  mutate(sd_pos = mean+sd, sd_neg = mean-sd)

ggplot(mat_final)+
  #geom_point(aes(x=factor(agg), y= cor, col=factor(sci)))+
  xlab("aggregation period")+
  scale_color_discrete("SCI", labels=c("spi", "spei"))+
  #scale_x_continuous(breaks=c(1,2,3,6,12,24), labels=c("1","2","3","6","12","24"))+
  theme_bw()+
  ylab("spearman correlation of SCI[-1] ~ SSI-1")+
  geom_pointrange(aes(x=factor(agg), ymin = sd_neg, ymax=sd_pos,y=mean, col=factor(sci)), position = position_dodge(width=.3))

ggsave("./plots/sci_analysis/drought_predict1.pdf")


###
ccf_spi <- sci_ccf(sci= c(1,2,3,6,12,24),sci_namex = "spi_v2_", sci_namey="ssi_1")


ccf_spi[1]
ccf_spei <- sci_ccf(sci= c(1,2,3,6,12,24), sci_namex="spei_v2_", sci_namey="ssi_1")

#one can see that advancing the spei value leads to considerly less correlation after the spei_n lag (positive) the correlation drops dramatically. retropespective the correlation decreases slower. Meaning that spei leads ssi. 
# 

png("")
plot(x=1:catch_n, y=ccf_spi[[1]][,2])
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


# seasonal correlation values for SPI/SPEI####


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

boxplot(gauges$best_spei_summer ~ gauges$cor_spei_n)

gauges$best_spei_summer = best_spei_summer
gauges$cor_spei_summer = value_spei_summer
gauges$best_spi_summer = best_spi_summer
gauges$cor_spi_summer = value_spi_summer




gauges_df = as.data.frame(gauges)
ggplot()+
  geom_point(aes(y=lt_cor_spi$`24`, x=gauges_df$bfi, col=as.factor(gauges_df$cor_spi_n )))+
  ylab("cor. SPI-24 ~ SSI-1")+
  xlab("BFI")+
  scale_color_discrete("optim. \n SPI-n")

ggplot()+
  geom_point(aes(y=lt_cor_spi$`12`, x=gauges_df$bfi, col=gauges_df$Enzgsg_ ))+
  ylab("cor. SPI-12 ~ SSI-1")+
  xlab("BFI")+
  scale_color_continuous("Catchment \n Size [km�?]")
ggsave("./plots/5_choice/memoryeffect_24.png")

    
# SCI indice when there is drought ####

#-----> see script drought attribution


#monthly correlation ####
feb_sci_cor = monthly_sci(month=2, threshold = 0) 
mar_sci_cor = monthly_sci(month=3, threshold = 0) 
apr_sci_cor = monthly_sci(month=4, threshold = 0) 
mai_sci_cor = monthly_sci(month=5, threshold = 0)
jun_sci_cor = monthly_sci(month=6, threshold = 0) 
aug_sci_cor = monthly_sci(month=8,threshold = 0) 
sep_sci_cor = monthly_sci(month=9, threshold = 0) 
oct_sci_cor = monthly_sci(month=10, threshold = 0) 
nov_sci_cor = monthly_sci(month=11, threshold = 0) 

pdf("./plots/5_choice/cor_ssi_spi_winter_spring.pdf", width=7.6, height = 5.6)
monthly_cor_sci_spring(sr_x=0,sci_typex="spi")

dev.off()

pdf("./plots/sci_analysis/cor_ssi_spi_winter_aug_sep.pdf")
monthly_cor_sci_yr(sr_x=2,sci_typex="spi")
dev.off()


oct0 = ggplot()+
  geom_boxplot(data=oct_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==0), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("March")+
  ylab("spearman correlation SSI ~ SPI-n")+
  scale_x_discrete(labels=c(agg_month))+theme_bw()
nov0 = ggplot()+
  geom_boxplot(data=nov_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==0), aes(x=sci_type, y=cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("April")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))+theme_bw()
oct = ggplot()+
  geom_boxplot(data=oct_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==2), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("Oct")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))+theme_bw()
nov = ggplot()+
  geom_boxplot(data=nov_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==2), aes(x=sci_type, y=cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("Nov")+
  ylab("")+
  scale_x_discrete(labels=c(agg_month))+theme_bw()
  

grid.arrange(oct0,nov0,oct,nov, ncol=4)

zyp::confint.zyp()
ggplot()+
  geom_boxplot(data=feb_sci_cor %>% filter(str_detect(sci_type, sci_typex), sr==sr_x), aes(x=sci_type, y = cor), na.rm = T)+
  ylim(c(0,.9))+
  xlab("March")+
  ylab("spearman correlation ssi-1 ~ x")+
  scale_x_discrete(labels=c(agg_month))



mar_best_spi_cor = mar_sci_cor %>% 
  filter(str_detect(sci_type, "spi"))%>% 
  group_by(gauge) %>% 
  summarise(max_cor = max(cor) , spi_n = as.factor(sci_type[which.max(cor)]))%>% 
  mutate(spi_n = as.integer(str_remove(spi_n,"spi_")))

mar_best_spei_cor = mar_sci_cor %>% 
  filter(str_detect(sci_type, "spei"))%>% 
  group_by(gauge) %>% 
  summarise(max_cor = max(cor) , spei_n = as.factor(sci_type[which.max(cor)])) %>% 
  mutate(spei_n = as.integer(str_remove(spei_n,"spei_")))

gauges$mar_best_spei = mar_best_spei_cor$max_cor
gauges$mar_best_spei_n = mar_best_spei_cor$spei_n
gauges$mar_best_spi = mar_best_spi_cor$max_cor
gauges$mar_best_spi_n = mar_best_spi_cor$spi_n



#why some values are really low?

worst_spei =value_spei %>% order(., decreasing = FALSE) %>% head(10)
worst_spi =value_spi %>% order(., decreasing = FALSE) %>% head(10)
value_spei[worst_spei]
value_spi[worst_spi]

par(mfrow=c(2,1))

plot(mt_mn_q_wide$`72`, t="l", ylim=c(-2,2), ylab="72 SPI-6/q [m�/s]")
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


#sci analysis per bfi class ####
bfi_cor_ssi_0 = bfi_sci(threshold = 0)

bfi1 = ggplot()+
  geom_boxplot(data = filter(bfi_cor_ssi_0$`<.4`,str_detect(sci_type, "spi")), aes(x=sci_type, y=cor))+
  ylim(c(0,.7))


bfi2 = ggplot()+
  geom_boxplot(data = filter(bfi_cor_ssi_0$`.4-.6`,str_detect(sci_type, "spi")), aes(x=sci_type, y=cor))+
  ylim(c(0,.7))

bfi3 = ggplot()+
  geom_boxplot(data = filter(bfi_cor_ssi_0$`.6-.8`,str_detect(sci_type, "spi")), aes(x=sci_type, y=cor))+
  ylim(c(0,.7))

bfi4 = ggplot()+
  geom_boxplot(data = filter(bfi_cor_ssi_0$`.8-1`,str_detect(sci_type, "spi")), aes(x=sci_type, y=cor))+
  ylim(c(0,.7))
grid.arrange(bfi1,bfi2,bfi3,bfi4, ncol=4)

#plots####
  gauges_df = gauges %>% as.data.frame()
  
  ggplot() +
      geom_point(data= gauges_df, aes(x=bfi,y=mnq30_month,  col=as.factor(sr)))+
    ylab("average month of lowest flow")+
    scale_color_discrete("Seasonality \n ratio", label=c("summer", "unclear", "winter"))+
    scale_y_continuous(breaks = seq(1,12,1), labels=month.abb)
  
  ggsave("bfi_lf_month.png")
    

  
  #skewness#### 
  res=matrix(nrow = catch_n, ncol=length(agg_month))
  i=1
  for ( n in agg_month){
res[,i]= sapply(1:catch_n, function(x) moments::skewness(get(paste0("spei_v2_",n))[,x],na.rm = T ))
i=i+1
  }
  
  colMeans(res)
  
  #number of droughts of every catchment ####
  
which(spi_v2_24[,c(1:338)]< -1) %>% length()/338
  
  ssi_1_long %>% 
    filter(month(yr_mt) ==1) %>% 
    group_by(gauge) %>% 
    summarise(below = length(which(ssi < 0)))
    6/40
    
    spi_v2_1[,1] %>% mean()

  