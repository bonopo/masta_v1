
# Clustering --------------------------------------------------------------
source("./R/masta_v1/functions.R")# has to run before if not objects will be missin!
source("./R/masta_v1/data_handling.R")# has to run before if not objects will be missin!
source("./R/masta_v1/sci_calculation.R")# has to run before if not objects will be missin!
source("./R/masta_v1/drought_characteristics.R") # has to run before if not objects will be missin!

#seasonality ratio (SR)####

# creating two time series one winter one summer
#calculating q95 for both parts
q_sr_w <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month > 11 | month <4) %>%  #definitin of winter from Laaha et al 2006
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_w = mean(qt))

q_sr_s <- q_long %>%
  mutate(month = month(date)) %>%
  filter(month < 12 | month > 3) %>%
  group_by(gauge) %>%
  mutate(qt = quantile(q, 0.05)) %>%
  summarise(q95_s = mean(qt))

q_sr <- merge(q_sr_s, q_sr_w, by="gauge")
q_sr$sr <- q_sr$q95_s/q_sr$q95_w # SR is caclulated via q95_summer/q95_winter from Laaha et al 2006

q_sr$sr_value = NA
q_sr$sr_value[which(q_sr$sr < .9)] <- 0 #summer
q_sr$sr_value[which(q_sr$sr > 1.1)] <- 2 #winter NAs are produced in 9 time series that are very altered or have no clear seasonality
q_sr$sr_value[which(q_sr$sr >= .9 & q_sr$sr <= 1.1)] = 1 #no clear seasonality 

gauges$sr <- as.numeric(q_sr$sr_value)
#spplot(gauges, "sr")

# ind <- which(is.na(q_sr$sr_value))
# q_long %>%
#   filter(gauge %in% ind) %>%
#   filter(year(date) < 1975) %>%
#   ggplot() +
#   geom_smooth(aes(x=date, y=q, group= as.factor(gauge), color= as.factor(gauge)))+
#   scale_y_log10()

gauges$ezggr_class <- cut(gauges$Enzgsg_, breaks=c(0,50,100,150,Inf), labels=c("<50", "50-100", "100-150", "150-200"))


# BFI clustering according  ---------------------------------------------
bfi <- c()

for (i in 1:catch_n){
lf_obj <- q_long %>% 
  filter(gauge == i) %>% 
  mutate( flow= q,day = day(date), month=month(date), year = year(date)) %>% 
  dplyr::select(-date, -gauge, -q) %>% 
  as.data.frame()
 
basefl <- createlfobj(x= lf_obj, hyearstart = 1, baseflow = T)
bfi[i] <- BFI(basefl)
}
plot(bfi)
gauges$bfi <- bfi

#SAAR ####
#standart climate period 1971 bis 2000 (see DWD)
# standart period averae annual rainfall
saar <- precip_long %>% 
  filter(year(date) >1970 & year(date) < 2001) %>% 
  group_by(gauge) %>% 
  summarise(sum_mm_yr = sum(sum_mm)/30)

gauges$saar <- saar$sum_mm_yr

#median drought duration ####
  median_drought_duration = c()
  for (g in 1:catch_n){
    median_drought_duration[g] = dsi_1[[g]]$dr_length %>% median()
  }

gauges$med_dr_dur = median_drought_duration

#maximum duration ####
max_drought_duration = c()
  for (g in 1:catch_n){
    max_drought_duration[g] = dsi_1[[g]]$dr_length %>% max()
  }

gauges$max_dr_dur = max_drought_duration
#median severity####
median_drought_severity = c()
  for (g in 1:catch_n){
    median_drought_severity[g] = dsi_1[[g]]$dsi %>% median()
  }

gauges$med_dr_sev = median_drought_severity

#median intensity ####

median_drought_inten = c()
  for (g in 1:catch_n){
    median_drought_inten[g] = dsi_1[[g]]$dr_intens %>% median()
  }

gauges$med_dr_int  =median_drought_inten
#maximum severity####
max_drought_sev = c()
  for (g in 1:catch_n){
    max_drought_sev[g] = dsi_1[[g]]$dsi%>% min() #min!!!!!!!!!!!
  }

gauges$max_dr_sev  =max_drought_sev


#total number of events####

n_events = c()
  for (g in 1:catch_n){
    n_events[g] = dsi_1[[g]]$event_n %>% max()
  }

gauges$n_events = n_events

#average q ####

q_mean = q_long %>% 
  filter(year(date) >1970 & year(date) < 2001) %>% 
  group_by(gauge) %>% 
  summarise(mean = mean(q))

gauges$q_mean = c(q_mean$mean)
#clustered plots #### 
#see barker et al

gauges_df = as.data.frame(gauges)

head(gauges_df)

ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= n_events, alpha= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= best_spi, alpha= bfi))


ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= best_spei, col= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= cor_spi, alpha= bfi))
ggsave("best_spi_cor_spi_saar.png")

ggplot(data=gauges_df)+
  geom_point(aes(x=saar, y= cor_spei, col= bfi))
ggsave("bfi_cor_spei_saar.png")
ggplot(data=gauges_df)+
  geom_point(aes(x=Enzgsg_, y= med_dr_sev, col= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=max_dr_sev, y= n_events, col= bfi))

ggplot(data=gauges_df)+
  geom_point(aes(x=n_events, y= max_dr_dur, col = bfi))
ggsave("max_dr_dur_med_dr_int_bfi.png")

png("gauges_sr.png")
spplot(gauges, "sr")
dev.off()

gauges$cor_spi_n

gauges$sr

#regionalisation####
int = which(gauges$Hochwrt > 5900000)
gauges$saar[int] %>% mean()
gauges$saar %>%  which.max()

spplot(gauges, "saar")
