
# Trend analysis ----------------------------------------------------------

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")

# mann- kendall test ------------------------------------------------------


ken_spei <- ken_trend(sci_name = "spei_v2")
ken_spi <- ken_trend(sci_name="spi_v2")
ken_ssi <- ken_trend(sci_name= "ssi")

# quantil trend ####
qua_trend <- function(quantil=0.1){
  ken=matrix(nrow=catch_n, ncol=2)
  data <- q_long %>%
  mutate(year = year(date)) %>% 
  group_by(gauge, year) %>%
  summarise(qt=quantile(q, quantil)) %>% 
    ungroup()
  for (g in 1:catch_n){
  res <- MannKendall(data[data$gauge == g,]$qt)
  ken[g,1] <- res$tau
  ken[g,2] <- res$sl
  }
  ken <- as.data.frame(ken)
  colnames(ken) <- c("tau", "sl")
  return(ken)
  }

quant_trend_1 <- qua_trend(quantil = 0.1)
quant_trend_05 <- qua_trend(quantil = 0.05) # wie schweizer defnition Q347
pdf("./plots/mk_quant.pdf")
plot(quant_trend_05$tau, ylab="tau", xlab="catchments", ylim=c(-0.65, .45))
points(quant_trend_1$tau, col=2)
legend("bottomleft", pch=c(1,1), col=c(1,2), c("quantil = .05", "quantil = .1"), bty="n")
abline(h=0, lty=2, col=4)
dev.off()

# temperature trends ####
begin = 4
end = 10
paste0(value,"_",method) 

# precipitation trends ####
rename_mp
quote()