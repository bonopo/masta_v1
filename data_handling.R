# Startin script
# Preambel ----------------------------------------------------------------
setwd("C:/Users/Menke/Dropbox/masterarbeit/R")
# save.image(file="./data/r_temp_image/basic.Rdata")
load(file="./data/r_temp_image/basic.Rdata")
load(file="./output/hydro_year.Rdata")
load(file="./output/gauges.Rdata")
load(file="./output/ms7.Rdata", verbose = T)
load(file="./output/fs.Rdata", verbose = T)
#install.packages(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI", "tweedie", "SPEI", "eha","reliaR", "PearsonDS","FAdist","trend", "Kendall","mgcv", "modiscloud", "Hmisc", "scales", "sn", "randomForest", "gridExtra", "foreach",  "doSNOW", "snow", "itertools","beeswarm"))
# install.packages("drought", repos="http://R-Forge.R-project.org")
#install.packages("beeswarm")
sapply(c("raster", "rgdal", "tidyverse", "magrittr", "reshape2", "SCI",  "lubridate", "SPEI", "lmomco",  "evd", "reliaR", "PearsonDS", "FAdist","trend","Kendall", "mgcv", "lmtest","lfstat", "modifiedmk", "climtrends", "boot", "parallel","modiscloud", "Hmisc","car", "scales", "sn", "gridExtra",  "foreach", "doSNOW", "snow","beeswarm"), require, character.only = T)
#install.packages("climtrends", repos="http://R-Forge.R-project.org")


# Load data ---------------------------------------------------------------


source("./R/masta_v1/functions.R")
#precipiatation
load("./data/catchments/eobs_pr_part.Rdata", verbose = T) 
# temperature
load("./data/catchments/eobs_temp_part.Rdata", verbose = T) 
# streamflow
load("./data/catchments/streamflow.Rdata", verbose = T)
#shapefile
#gauges  <- shapefile("./data/raster/gauges") #
#landuse data
legende <- read.csv("./data/geo_landuse/clc_legend.csv", sep=";", header=T)[,c(1,5)]
legende2 <- read.csv("./data/geo_landuse/clc_legend.csv", sep=";", header=T)[,c(1,3:5)]
landuse_v1 <- read.csv("./data/geo_landuse/LaNu_per_EZG.csv")
#hydrogeo data
hydrogeo <- read.csv("./data/geo_landuse/hydrogeo.csv")
# germany shapefile
germany = raster::getData("GADM",country="Germany",level=0)
#North atlantic oscillation 
nao_raw=read.table("./data/catchments/nao_monthly.txt")
# User defined constants --------------------------------------------------

date_seq <- seq.Date(from= ymd("1970-01-15"), to = ymd("2009-12-15"), by="month")  
date_seq_long =seq.Date(from= ymd("1970-01-01"), to = ymd("2009-12-31"), by= "day")
catch_n <- 337 # number of catchments; originally 338 but last catchment has been altered, the water gauge has not been mantained which caused it to be filled up over the time
no_cores = detectCores() #for parallel computing
my_catch = colnames(precip)
agg_month =c(1, 2, 3, 6, 12, 24)

nice <- theme_bw()+
    theme(legend.position = "bottom",
          text = element_text(size = 12))  ##ggplot theme

# transforming data ----------------------------------------------------------------

#precip####
#removing last column see commentary line 30 for explanation
precip = precip[,c(1:catch_n)]
colnames(precip) <- 1:catch_n
precip_long <- load_file(precip, "sum_mm")
unique(precip_long$gauge)
mt_sm_p <- precip_long %>% #monthly (calender based) sum
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>%
  group_by(gauge,yr_mt) %>%
  summarise(month_sum = sum(sum_mm)) %>%
  ungroup()

remove(precip)
mt_sm_p_wide <- spread(mt_sm_p, key=gauge, value=month_sum, drop=F) %>% dplyr::select(-yr_mt) %>% as.data.frame()


#discharge####
streamflow = streamflow[,c(1:catch_n)]
q_long <- load_file(streamflow, "q")
q_wide <- spread(q_long, key= gauge, value = q)
q_wide %<>% dplyr::select(-date) %>% as.data.frame()
colnames(q_wide) <- c(1:catch_n)
mt_mn_q <- q_long %>% 
    mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>% 
  group_by(gauge,yr_mt) %>% 
  summarise(q_mean = mean(q)) %>% 
  ungroup() %>% 
  mutate(month = month(yr_mt))

mt_mn_q_wide <- spread(mt_mn_q, key = gauge, value = q_mean) %>% dplyr::select(-c(yr_mt,month)) %>% as.data.frame()

#temperature####
tempera = tempera[,c(1:catch_n)]
colnames(tempera) <- 1:catch_n
temp_long <- load_file(file=tempera, value_name = "temp", origin = "1950-01-01")
temp_long %<>% filter(date>= "1970-01-01" & date <= "2009-12-31") 
da_temp_wide = temp_long %>% 
  spread(key=gauge, value=temp) %>% 
  dplyr::select(-date) %>% 
  as.data.frame()

mt_mn_temp <- temp_long %>%
  mutate(yr_mt =  ymd(paste0(year(date),"-", month(date),"-","15"))) %>%
  group_by(gauge, yr_mt) %>%
  summarise(temp_m = mean(temp)) %>%
  ungroup()

mt_mn_temp_wide <- spread(mt_mn_temp, key = gauge, value = temp_m) %>% dplyr::select(-c(yr_mt)) %>% as.data.frame()




remove(tempera,streamflow)

#landuse####
colnames(legende) <- c("ID","LaNu")
landuse_v1 <- cbind(0:255,landuse_v1)
colnames(landuse_v1)[1] <- "ID"
gesamt <- merge(legende,landuse_v1, by="ID", all=F)[,-1]
colnames(gesamt)[-1] <- unlist(strsplit(colnames(gesamt)[-1],split=".N.10.0"))
summen <- colSums(gesamt[,-1])
gesamt[,-1] <- gesamt[,-1]/rep(summen, each=dim(gesamt)[1])
aussort <- which(rowSums(gesamt[,-1])==0)
gesamt <- gesamt[-aussort,]
ebenen <- merge(legende2,gesamt, by.x="LABEL3", by.y="LaNu") #choosing the label 1, it has the least levels (7) vs label 1 that has 19. Creating a too big variance
verallge <- function(x,ind){
  tapply(x,ebenen[,ind],sum)
}
ebene3 <- apply(ebenen[,-(1:4)],MARGIN = 2,verallge, ind=3)
landuse <- cbind(colnames(ebene3),apply(ebene3,MARGIN = 2,function(x) rownames(ebene3)[which.max(x)])) #selecting the dominating land use form
int= pmatch(my_catch,landuse[,1])
gauges$landuse = landuse[int,2]


# na_ign = is.na(int) %>% which() %>% my_catch[.] 
# na_ign %in% colnames(gesamt) # three catchments have no landuse
remove(legende, landuse_v1, summen, gesamt, legende2, ebenen,aussort, int, landuse, ebene3)

#hydro geology #####
gauges$hydrogeo = hydrogeo$Hydrogeologie #K= kl�fte P= poren alles andere ist gemisch KA= karst M=what is 
gauges$hydrogeo_simple = "other"
gauges$hydrogeo_simple[which(hydrogeo$Hydrogeologie == "P")] = "p"
gauges$hydrogeo_simple[which(hydrogeo$Hydrogeologie == "K")] = "f"
which(gauges$hydrogeo_simple == "f") %>% length() #fractured
which(gauges$hydrogeo_simple == "p") %>% length() # porous
which(gauges$hydrogeo_simple == "other") %>% length()
#shapefile adjustment####
# removing catchment 338
gauges= gauges[c(1:catch_n),]
remove(hydrogeo)

#possibly extending of time series? ####
sum(ymd(gauges$Ztrhnbg)<ymd("1960-1-2")) #if time series would be extended to 1.1.1960 there would be 201 catchments
table( year(ymd(gauges$Ztrhnbg)))



# PET calculation with thornwaite -----------------------------------------


#Gauss Krueger converted to WGS84
xy_gk <- cbind.data.frame("x_gk" = 4475806, "y_gk"= gauges$Hochwrt) #only latitude is needed, therefore random x value
coordinates(xy_gk) <-  c("x_gk", "y_gk")
proj4string(xy_gk) <- CRS("+proj=tmerc +lat_0=0 +lon_0=9 +k=1 +x_0=3500000 +y_0=0 +ellps=bessel +datum=potsdam +units=m +no_defs")
xy_wgs84 <- spTransform(xy_gk, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))


latitude <- coordinates(xy_wgs84) %>%
  as.data.frame() %>%
  dplyr::select(y_gk) %>%
  cbind("gauge" = unique(mt_mn_temp$gauge)) %>%
  as.tbl()

#calculating monthly Potential Evapotranspiration with thornthwaite in mm
pet_th <- list(NA)
for(i in unique(mt_mn_temp$gauge)){
  data <- mt_mn_temp$temp_m[mt_mn_temp$gauge==i]
  res_ts <- ts(data, frequency=12, start=c(1970,1))
  pet_th[[i]] <- thornthwaite(data,latitude$y_gk[latitude$gauge==i] )
}


# calculating SPEI: P - PET -----------------------------------------------

pet_th_vec <- pet_th[unique(mt_mn_temp$gauge)] %>%
  unlist() %>%
  as.numeric()


spei_data <- mt_sm_p %>%
    mutate(pet_th = pet_th_vec) %>%
    mutate(p_pet = month_sum - pet_th) 
# dx= spei_data %>% filter(.,gauge==1)
# plot(x=dx$yr_mt, y=dx$pet_th, type="l")


spei_data_mat <- spei_data %>% dplyr::select(gauge,yr_mt, p_pet) %>% spread(gauge, p_pet) %>% dplyr::select(-yr_mt) %>% as.data.frame() %>% 
set_colnames(1:catch_n)

year_p_pet = spei_data_mat

remove(pet_th, latitude,pet_th_vec)
remove(data, i, res_ts, xy_gk, xy_wgs84)


#nao ####

nao= nao_raw %>% 
  as.tbl() %>% 
  set_colnames(c("year", "month","nao")) %>% 
  filter(year >= 1970 & year <= 2009)


 
 #defining hydrological year beginning on 1.april because then there are no droughts (see plot below)
gauges$mnq30_month %>% plot()

year <- as.numeric(format(date_seq_long, "%Y"))
  begin_year <- year[1]
  end_year <- year[length(year)]  
  begin_date <- date_seq_long[1]  
  end_date <- date_seq_long[length(date_seq_long)]
  
  breaks <- seq(as.Date(begin_date), length = end_year - begin_year +2, by="year")
  hydro_year <- cut(date_seq_long, breaks = breaks, labels= c((begin_year +1):(end_year +1)))
  

length(hydro_year)
hydro_year = as.character(hydro_year) %>% as.integer()
hydro_year_wi = c(rep(1970,4),rep(1971:2009, each=5))
#save(list= c("hydro_year","hydro_year_wi"), file = "./output/hydro_year.Rdata")
remove(year, begin_year, end_year, begin_date, end_date, breaks)

#hydro year actually not used in further analysis rather the calender year

