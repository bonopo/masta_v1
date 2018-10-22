
# Trend analysis ----------------------------------------------------------

source("./R/masta_v1/functions.R")
source("./R/masta_v1/data_handling.R")

# mann- kendall test ------------------------------------------------------


ken_spei <- ken_trend(sci_name = "spei_v2")
ken_spi <- ken_trend(sci_name="spi_v2")
ken_ssi <- ken_trend(sci_name= "ssi")

# quantil trend ####