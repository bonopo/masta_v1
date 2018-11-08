
# old drought characterisation -----------------------------------------------------------

year = year(date_seq) %>% list()
mt_mn_q$year <- unlist(rep(year, times = catch_n))
mnq30 <- aggregate(mt_mn_q_wide, by= year, FUN= min, by.column=T)

mnq30_long <- gather(mq30, key=gauge, value= mq30, -date ) %>% as.tbl()

#mnq30 date

mnq30_date <- mnq30_long %>% 
  group_by(year(date), gauge) %>% 
  summarise(date[which.min(mq30)]) %>% 
  ungroup()

colnames(mnq30_date) <- c("year", "gauge", "date_mnq30")

#measure of distance to june to overcome problem 12 - 1####
 
plot(x= dr_beg[[15]]$mon_min, ylim= c(4,12), type="p")
 
 6-dr_beg[[i]]$mon_min[1:10] %>% mean() # for decade
 # you get weird averages
 
#with month with most severe ssi ?
 
 
 ssi_wide %>% 
   mutate(year = year(date)) %>% 
   filter(gauge == i, ssi < -1) %>% 
   group_by(year) %>% 
   summarise(which.min(ssi))
 



 
 
 
 




 
 # (OLD) SPEI vs. SPI comparison with regression (OLD)-------------------------------------------------

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



# functions ---------------------------------------------------------------

dist_fitt <- function(distry, monthy){ #similar as above, old version, not used in script
  q_by_month <- data.frame()
    for (i in monthy){
    q_by_month <- month_ext(monthx = monthy)
    assign(str_to_lower(month.abb[i]), q_by_month)
    }
    temp <- fitSCI(q_by_month$V1, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
  assign(paste0("params_", disrty), temp)
}

