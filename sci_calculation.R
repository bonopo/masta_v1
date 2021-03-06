
# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
# source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missing!


# Distribution free calculation -------------------------------------------

# sci_np(sci_data="mt_sm_p_wide", agg_n=c(1,2,3,6,9,12,24), sci_name="spi")
# sci_np(sci_data="spei_data_mat", agg_n=c(1,2,3,6,9,12,24), sci_name="spei")  
sci_np(sci_data="mt_mn_q_wide", agg_n=1, sci_name="ssi") 
ssi_1_long = ssi_1 %>% 
gather(key=gauge, value=ssi, -yr_mt) %>% 
  as.tbl()

# png("spi_1.png")
# plot(sort(spi_1$`1`), xlab="month [n]", ylab="spi-1 value" )
# dev.off()  
# SPI calculation (gamma distribution)  ---------------------------------------------------------
#calculating SPI with gamma distribution see paper McKee et al 1993 and (SCI Package)
# for (i in c(1,2,3,6,12,24)){ 
#   
# temp <- sci_calc(datax = mt_sm_p$month_sum, gaugex =mt_sm_p$gauge, distx = "gamma", agg_n = i )
# spi_v <- spei_vec(temp)
# m1 <- matrix(spi_v, nrow = 480, byrow =F)
# spi_df <- as.data.frame(m1)
# assign(paste0("spi_v3_",i), spi_df)
# }

#calculate SPI with spei package and gamma

for (n in agg_month){ 
res <- SPEI::spi(data= mt_sm_p_wide, scale=n)
m1 <- matrix(as.numeric(unclass(res)$fitted), nrow = 480, byrow =F)
if(any(is.infinite(m1))) {
     m1[which(is.infinite(m1))] <- NA}
m1 %<>% as.data.frame()
colnames(m1)=1:catch_n
assign(paste0("spi_v2_",n),m1)
}



# SPEI calculation with loglogistic distribution--------------------------------
#with sci package and log logistic #problem log logistic distribution not found!

# for (i in c(1,2,3,6,12,24)){ 
#   
# temp <- sci_calc(datax = mt_sm_p$month_sum, gaugex =mt_sm_p$gauge, distx = "loglogis", agg_n = i )
# spi_v <- spei_vec(temp)
# m1 <- matrix(spi_v, nrow = 480, byrow =F)
# spi_df <- as.data.frame(m1)
# assign(paste0("spei_v3_",i), spi_df)
# }

#with spei package and log logistic

for (n in c(1,2,3,6,12,24)){ 
res <- SPEI::spei(data= spei_data_mat, scale=n)
m1 <- matrix(as.numeric(unclass(res)$fitted), nrow = 480, byrow =F)
if(any(is.infinite(m1))) {
     m1[which(is.infinite(m1))] <- NA}
m1 %<>% as.data.frame()
colnames(m1)=1:catch_n
assign(paste0("spei_v2_",n),m1)
}


 remove(m1)



#comparison

# plot(x=date_seq, y=spi_v3_12$V1, t="l")
# lines(y=spi_v2_12$V1, x=date_seq,col=2)
# 
# png("para_nonpara.png", width = 1000, height=500)
# plot(x=spi_v3_12$V1 ,y= spi_12$`1`, xlab="parametric spi-12", ylab="non-parametric spi-12")
# abline(a=0, b=1)
# dev.off()

#v2 (with spei package) has lower values vor negative spi but very similar to v3
#v1 (non parametric) less neagtive values, doesn't represent extremes
 
 #parametric ssi calcualtion ####
 mt_mn_q
 ssi_p3 <- sci_calc(datax = mt_mn_q$q_sum, gaugex = mt_mn_q$gauge, distx = "pe3", agg_n = 1, p0x = F ) 
  ssi_wb <- sci_calc(datax = mt_mn_q$q_sum, gaugex = mt_mn_q$gauge, distx = "weibull", agg_n = 1, p0x = F ) 
ssi_gb <- sci_calc(datax = mt_mn_q$q_sum, gaugex = mt_mn_q$gauge, distx = "gumbel", agg_n = 1, p0x = F )
ssi_ln <- sci_calc(datax = mt_mn_q$q_sum, gaugex = mt_mn_q$gauge, distx = "lnorm", agg_n = 1, p0x = F )
mt_sum_q$ssi_p3 <- spei_vec(ssi_p3, gaugex = mt_mn_q$gauge)
mt_sum_q$ssi_wb <- spei_vec(ssi_wb, gaugex = mt_sum_q$gauge)
mt_sum_q$ssi_gb <- spei_vec(ssi_gb, gaugex = mt_sum_q$gauge)
mt_sum_q$ssi_ln <- spei_vec(ssi_ln, gaugex = mt_sum_q$gauge)
 