
# User defined functions --------------------------------------------------

# loading function

load_file <- function(file, value_name, origin="1970-1-1"){
  output <- melt(file, varnames = c("date", "gauge"), value.name = value_name )
  seq_date <- seq.Date(from= as.Date(origin),by=1, length.out = diff(range(output$date))+1) %>% 
  rep(., times=length(unique(output$gauge)))
  output %<>%
  mutate(date = seq_date) %>% 
  mutate(gauge = parse_number(gauge)) %>% 
  mutate(gauge =as.integer(gauge)) %>% 
  as.tibble()
  return(output)
}

sci_calc <- function(datax = spei_data$p_pet, gaugex=spei_data$gauge, distx="gev", agg_n=6, p0x=F){ #fit monthly values to certain distribution and calculate indice
 # 1. Step: fitting of distribution to aggregated data with SCI package
 output <- as.list(NA)
for(i in unique(gaugex)){
data <- datax[gaugex==i]
params_dist <- fitSCI(data, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
#inital values for parameters calculated with L-moments and then optimization with maximum likelihood
#if there are a lot of zeros in the time series it is recommended to use p0=TRUE
# 2. Step: transform to standart normal distribuion with mean: 0 and sd: 1
spi_temp <- transformSCI(data, first.mon = 1, obj = params_dist)
output[[i]] <- spi_temp
}
 return(output)
}

# dist_fitt <- function(distry, monthy){ #similar as above, old version, not used in script
#   q_by_month <- data.frame()
#     for (i in monthy){
#     q_by_month <- month_ext(monthx = monthy)
#     assign(str_to_lower(month.abb[i]), q_by_month)
#     }
#     temp <- fitSCI(q_by_month$V1, first.mon = 1, distr = distx, time.scale = agg_n, p0 =p0x)
#   assign(paste0("params_", disrty), temp)
# }

spei_vec <- function(data, spei=FALSE, gaugex=mt_mn_temp$gauge){ #to transform spei or spi list into vector
  output <-  vector()
  for(i in unique(gaugex)){
  temp <- unclass(data[[i]])
  if(spei == FALSE){
    output <- c(output, as.numeric(temp))}else{
    output <- c(output, as.numeric(temp$fitted))}
  }
  return(output)
}
y <- 1970
g <- 1

month_ext <- function(monthx = 1, datax = mt_mn_q){#month extraction for SSI calculation to calculate SSI for each month individually
    output <- data.frame()
    data <- datax %>% 
      filter(month == monthx)
    for (g in unique(data$gauge)){
      for (y in seq(min(year(datax$yr_mt)),max(year(datax$yr_mt)),by = 1)){
        output[y,g] <- data$q_mean[data$gauge == g & year(data$yr_mt) == y]
      }
    }
    output_short <- output[min(year(datax$yr_mt)):max(year(datax$yr_mt)),]
    return(output_short)
}

ken_trend <- function(sci= seq(1:agg_month), sci_name="spei_v2"){
  ken_tot <- list()
  ken_tau <- data.frame()
  ken_sl <- data.frame()
  for (i in sci){
  if(sci_name == "ssi"){
  sci_data <- ssi_sorted }else{
  sci_data <- get(paste0(sci_name,"_",i))}
  for(g in 1:ncol(sci_data)){
  res <- MannKendall(sci_data[,g])
  ken_tau[g,i] <- res$tau
  ken_sl[g,i] <- res$sl
  }}
  if(sci_name == "ssi"){
    ken_tot <- cbind(ken_tau[,1], ken_sl[,1])
    colnames(ken_tot) <- c("tau", "sl")
  }else{
  colnames(ken_tau) <- as.character(sci)
  colnames(ken_sl) <- as.character(sci)
  ken_tot[[1]] <- ken_tau
  ken_tot[[2]] <- ken_sl}
  return(ken_tot)
}

sci_ccf <- function(sci= seq(1:agg_month), sci_namex="spei_v2", sci_namey="ssi_sorted"){
  ccf_tot <- list()
  ccf_acf <- data.frame()
  ccf_lag <- data.frame()
  y <- get(sci_namey)
  for (i in sci){
  sci_data <- get(paste0(sci_namex,"_",i))
  for(g in 1:ncol(sci_data)){
    if(any(is.infinite(sci_data[,g]))) {
     sci_data[,g][ which(is.infinite(sci_data[,g]))] <- NA}
  ccf_temp <- ccf(x= sci_data[,g], y= y[,g], na.action = na.pass, plot = FALSE)
  ccf_acf[g,i] <- max(ccf_temp$acf)
  ccf_lag[g,i] <- ccf_temp$lag[which.max(ccf_temp$acf),1,1]
  }}
  colnames(ccf_lag) <- as.character(sci)
  colnames(ccf_acf) <- as.character(sci)
  ccf_tot[[2]] <- ccf_lag
  ccf_tot[[1]] <- ccf_acf
  return(ccf_tot)
}

spi_spei_reg <- function(sci_n = agg_month){
  lm_intercept <- data.frame()
  lm_slope <- data.frame()
  lm_rsq <- data.frame()
  lm_res <- list()
  for (n in 1:sci_n){
      x <- get(paste0("spei_v2_",n))
      y <- get(paste0("spi_v2_",n))
      for (g in 1:ncol(x)){
        temp <- lm(x[,g]~y[,g], na.action = na.exclude)
        lm_intercept[g,n]  <- temp$coefficients[1]
        lm_slope[g,n] <- temp$coefficients[2]
        lm_rsq[g,n] <- summary(temp)$adj.r.squared
    }
  }
  colnames(lm_intercept) <- 1:agg_month
  colnames(lm_slope) <- 1:agg_month
  colnames(lm_rsq) <- 1:agg_month
  lm_res[[1]] <- lm_intercept
  lm_res[[2]] <- lm_slope
  lm_res[[3]] <- lm_rsq
return(lm_res)
}
sci_reg <- function(sci_n = 2, pred="spei_v2", resp="ssi_sorted", pred2="spi_v2", interaction=FALSE){
  lm_res <- list()
      x <- get(paste0(pred,"_",sci_n))
      x2 <- get(paste0(pred2,"_",sci_n))
      y <- get(paste0(resp))
      for (g in 1:ncol(x)){
        if(interaction==FALSE){
        lm_res[[g]] <- lm(y[,g]~x[,g], na.action = na.exclude) %>% summary()}else{
        lm_res[[g]] <- lm(y[,g]~x[,g] *x2[,g], na.action = na.exclude) %>% summary()}
      }
  return(lm_res)}


cor_sci_ssi <- function(sci_n= c(1:12), cor_met="p", sci="spi_v2_", ssi="ssi_sortet"){
mat <- matrix(ncol=agg_month, nrow=length(ssi_sorted))
    y_data <- get(ssi)
    for (n in sci_n){
    x_data <- get(paste0(sci,n))
    for (g in 1:length(ssi_sorted)){
    mat[g,n] <- cor(x= x_data[,g], y_data[,g], method = cor_met, use="na.or.complete" )
    }}

return(mat)
}

# non parametric sci calculation ------------------------------------------


sci_np <- function(sci="mt_sm_p", n=1, method="mean"){
 erg <- matrix(nrow=480, ncol=catch_n)
 data <- get(sci)
 if(n>1){ 
   data <- rollapply(data, width=n, FUN=method, by.column =TRUE, align="right", fill=NA)}
  for(i in 1:catch_n){
# r_test <- rank(x= test_2, na.last = T, ties.method = "f")
erg[,i] <- CTT::score.transform(data[,i], mu.new = 0, sd.new = 1, normalize = TRUE)$new.scores 
  }
 erg[which(is.infinite(erg))] <- NA
  return(erg)
}

